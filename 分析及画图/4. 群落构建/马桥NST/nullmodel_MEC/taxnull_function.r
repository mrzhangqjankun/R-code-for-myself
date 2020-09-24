taxnull<-function(comm,treat = NULL,dist.method="jaccard",abundance.weighted=FALSE, 
				rand=1000,nworker=4,memory.G=50,trace=TRUE,
                betadisper.type="centroid", Dmax=1, perm.rand=rand)
{
  # version 4: 2018.4.21
  # Input:
  # comm: a matrix or data.frame, each row as a sample and each column as an OTU or gene. number should be integer.
  # treat: a matrix or data.frame, each row as a sample and each column defines a set of treatments.
  # dist.method: dissimilarity index method, pass to function vegdist in R package vegan.
  # abundance.weighted: logic, weighted or not.
  # rand: integer, randomization time.
  # nworker: parallel computing thread. if your computer CPU has 8 cores, please use no more than 8 nworker.
  # memory.G: to set memory upper limit. If your computer only have 4 Gb but you need 50 Gb, you may set memory.G=50, to use hard disk as memory.
  # trace: logic, whether to show message when calculating
  # betadisper.type: "median" or "centroid", pass to function betadisper in R package vegan.
  # Dmax: numeric, the upper limit of the dissimilarity index. For many popular indexes, Dmax=1.
  
  # Null model algorithm: binary part is based on Chase et al 2011; abundance-weighted part is based on Stegen et al 2013.
  
  # Output:
  # RC: modified Raup-Crick metric is based on Chase et al 2011 Ecosphere and Stegen et al 2013 ISME J.
  # SES: Standard Effect Size is based on the equation for betaNTI (Fine et al 2011 Ecography). Also applied in Kraft et al 2011 Science.
  # ST.Ratio.old: stochasticity ratio according to Zhou et al 2014 PNAS.
  # ST.Ratio: modified from Zhou et al 2014 PNAS. Different from Zhou's previous method which only consider observed similarity higher than null expectaction, this ST considered the situation where observed similarity could be lower than null expectation.
  # Multivariate dipersion analysis based on null model as described in Zhou et al 2014 PNAS. (Also see function betadisper in R package vegan)
  ## Disp.summary: summary of the mean distance to centroid (or median point) in each treatment.
  ## Disp.dist.obs: observed distance from each sample to the centroid (or median point) of its treatment. 
  ## Disp.dist.null: null expected distance from each sample to the centroid (or median point) of its treatment. 
  # beta.obs: observed dissimilarity (beta diversity) index values
  # beta.rand: null dissimilarity index values
  # summary: summary of all statistics for each treatment.
  ## StochasticityRatio.mean, sd, min, 0.25, median, 0.75, max: mean, standard deviation, and quantiles of ST.Ratio within each treatment
  ## Dissimilarity.mean: mean value of observed dissimilarity within each treatment.
  ## Similarity.mean,sd, min,0.25, median, 0.75, max: mean, standard deviation, and quantiles of observed similarity within each treatment.
  ## Dissimilarity.Null.mean: mean value of null expected dissimilarity within each treatment.
  ## Similarity.Null.mean,sd, min,0.25, median, 0.75, max: mean, standard deviation, and quantiles of null expected similarity within each treatment.
  ## STratio.RC: Percentage of turnovers without significant RC (-0.95<RC<0.95)
  ## RC.mean, RC.sd: mean and standard deviation of RC values.
  ## STratio.SES: Percentage of turnovers without significant SES (-1.96<SES<1.96)
  ## SES.mean, SES.sd: mean and standard deviation of SES values.
  ## Disp.distance.mean, sd: mean and standard deviation of observed distance to centroid (or median point) in each treatment based on multivariate dispersion analysis.
  ## Disp.distance.Null.mean, sd: mean and standard deviation of null expected distance to centroid (or median point) in each treatment.
  ## F.value: F value of ANOVA between the observed and null expected distance to centroid (or median point) for each treatment
  ## permANOVA.p: permutation ANOVA test based on the F value
  ## NullModel.p: p value based on difference of the mean distance to centroid between observed and all null results.
  # PERM.test: permutational multivariate ANOVA test for RC, SES, and ST.Ratio
  
  # References:
  # Chase JM, Kraft NJB, Smith KG, Vellend M, Inouye BD (2011). Using null models to disentangle variation in community dissimilarity from variation in alpha-diversity. Ecosphere 2: 1-11.
  # Stegen JC, Lin X, Fredrickson JK, Chen X, Kennedy DW, Murray CJ et al (2013). Quantifying community assembly processes and identifying features that impose them. Isme Journal 7: 2069-2079.
  # Zhou J, Deng Y, Zhang P, Xue K, Liang Y, Van Nostrand JD et al (2014). Stochasticity, succession, and environmental perturbations in a fluidic ecosystem. Proceedings of the National Academy of Sciences of the United States of America 111: E836-E845.
  # Fine PVA, Kembel SW (2011). Phylogenetic community structure and phylogenetic turnover across space and edaphic gradients in western Amazonian tree communities. Ecography 34: 552-565.
  # Kraft NJB, Comita LS, Chase JM, Sanders NJ, Swenson NG, Crist TO et al (2011). Disentangling the drivers of beta diversity along latitudinal and elevational gradients. Science 333: 1755-1758.

  # Step 1 # check input, calculate observed dissimilarity and dispersion.
  library(parallel)
  library(vegan)
  treat=as.matrix(treat)
  if(memory.limit()<memory.G*1024) memory.limit(size=memory.G*1024)
  com<-comm[,colSums(comm)>0,drop=FALSE]
  if(!is.null(treat)){if(sum(rownames(treat)!=rownames(comm))>0) stop("The row names of comm and treat not match.")}
  
  if(trace) message("Now calculating observed dissimilarity. ",date())
  BC.obs=as.matrix(vegdist(com,method = dist.method,binary = (!abundance.weighted)))
  
  if(!is.null(treat))
  {
    cdis.obs<-cdisj.obs<-cgroup<-list()
    for(i in 1:ncol(treat))
    {
      bdispi=vegan::betadisper(as.dist(BC.obs),group=treat[,i],type = betadisper.type)
      cdis.obs[[i]]=sapply(sort(unique(as.vector(treat[,i]))),function(tn){mean(bdispi$distances[which(bdispi$group==tn)])})
      cgroup[[i]]=bdispi$group
      cdisj.obs[[i]]=bdispi$distances
      names(cdis.obs[[i]])=paste0(colnames(treat)[i],".",names(cdis.obs[[i]]))
    }
    cdisobs=unlist(cdis.obs)
    cdisjobs=data.frame(Reduce(cbind,cdisj.obs),stringsAsFactors = FALSE)
    colnames(cdisjobs)<-names(cgroup)<-colnames(treat)
  }
  
  # Step 2 # Randomization based on null model algorithm, and calculate null expected dissimilarity and dispersion.
  
  sp.frequence=colSums(com>0)
  sp.ab=colSums(com)
  sp.num=ncol(com)
  
  samp.richness=rowSums(com>0)
  samp.ab=rowSums(com)
  samp.num=nrow(com)
  samp.name=rownames(com)
  
  
  BC.rand<-function(j,sp.frequence,sp.ab,sp.num,samp.richness,samp.ab,samp.num,samp.name,
                    abundance.weighted,treat,dist.method,trace,betadisper.type)
  {
    # this function is to do randomization and calculate dissimilarity and dispersion for randomized communities.
    library(vegan)
    if(trace) message("Now randomizing j=",j,". ",date())
    
    comr.b=t(matrix(sapply(1:samp.num,
                           function(i)
                           {
                             res=rep(0,sp.num)
                             id=sample(1:sp.num,samp.richness[i],replace = FALSE,prob = sp.frequence)
                             res[id]=1
                             res
                           }),nc=samp.num))
    
    if(abundance.weighted)
    {
      size.rand=samp.ab-samp.richness
      sp.id<-lapply(1:samp.num,function(i){which(comr.b[i,]>0)})
      comr<-t(matrix(sapply(1:samp.num,
                            function(i)
                            {
                              res=rep(0,sp.num)
                              if(length(sp.id[[i]])!=0)
                              {
                                if(length(sp.id[[i]])==1){ab.id=rep(sp.id[[i]],size.rand[i])}else{
                                  ab.id=sample(sp.id[[i]],size.rand[i],replace = TRUE,prob = sp.ab[sp.id[[i]]])
                                }
                                ab.table=table(ab.id)
                                res[as.numeric(names(ab.table))]=as.vector(ab.table)
                                res[sp.id[[i]]]=res[sp.id[[i]]]+1
                              }
                              res
                            }),nc=samp.num))
    }else{comr=comr.b}
    
    rownames(comr)=samp.name
    BCrand=as.matrix(vegdist(comr,method=dist.method,binary = (!abundance.weighted)))
    
    if(!is.null(treat))
    {
      cdis.rd<-cdisj.rd<-list()
      for(i in 1:ncol(treat))
      {
        bdispi=vegan::betadisper(as.dist(BCrand),group=treat[,i],type = betadisper.type)
        cdis.rd[[i]]=sapply(sort(unique(treat[,i])),function(tn){mean(bdispi$distances[which(bdispi$group==tn)])})
        names(cdis.rd[[i]])=paste0(colnames(treat)[i],".",names(cdis.rd[[i]]))
        cdisj.rd[[i]]=bdispi$distances
      }
      cdisrd=unlist(cdis.rd)
      cdisjrd=Reduce(cbind,cdisj.rd)
    }else{
      cdisrd=NA
      cdisjrd=NA
    }
    list(BCrand=BCrand,cdis.rd=cdisrd,cdisj.rd=cdisjrd)
  }
  
  if(nworker==1)
  {
    BC.rdl<-lapply(1:rand,BC.rand,sp.frequence,sp.ab,sp.num,samp.richness,samp.ab,samp.num,samp.name,
                   abundance.weighted,treat,dist.method,trace,betadisper.type)
  }else{
    c1<-parallel::makeCluster(nworker,type="PSOCK")
    if(trace) message("Now parallel computing. begin at ", date(),". Please wait...")
    BC.rdl<-parallel::parLapply(c1,1:rand,BC.rand,sp.frequence,sp.ab,sp.num,samp.richness,samp.ab,samp.num,samp.name,
                                abundance.weighted,treat,dist.method,trace=FALSE,betadisper.type)
    parallel::stopCluster(c1)
  }
  
  BC.rd=lapply(1:length(BC.rdl), function(i){BC.rdl[[i]]$BCrand})
  
  # Step 3 # Summarize dispersion analysis results and test significance.
  if(!is.null(treat))
  {
    eps=sqrt(.Machine$double.eps)
    cdis.rd=lapply(1:length(BC.rdl), function(i){BC.rdl[[i]]$cdis.rd})
    cdisrd=Reduce(rbind,cdis.rd);rownames(cdisrd)=c()
    xxm=matrix(cdisobs,nr=rand,nc=length(cdisobs),byrow=TRUE)
    cdis.p=(colSums(cdisrd<=(xxm+eps))+1)/(nrow(xxm)+1)
    cdisrdmm=colMeans(cdisrd,na.rm = TRUE)
    idxx=which(cdisrdmm<cdisobs)
    cdis.p[idxx]=((colSums(cdisrd>=(xxm-eps))+1)/(nrow(xxm)+1))[idxx]
    
    cdisj.rd=array(unlist(lapply(1:length(BC.rdl), function(i){BC.rdl[[i]]$cdisj.rd})),dim=c(nrow(com),ncol(treat),rand))
    cdisj.rdm=apply(cdisj.rd, c(1,2), mean,na.rm=TRUE)
    rownames(cdisj.rdm)=rownames(cdisjobs);colnames(cdisj.rdm)=colnames(cdisjobs)
    cd.summ=data.frame(Observed.Dispersion=cdisobs,Null.Dispersion=cdisrdmm)
    
    dispsum<-list()
    k=1
    
    poneanova<-function(V,grp,rand=999)
    {
      # this function just does permuation test of one way ANOVA
      V=as.numeric(V)
      grp=as.factor(grp)
      grp.lev=levels(grp)
      sampn=length(V)
      aovs=aov(V~grp)
      aovss=summary(aovs)
      Fobs=aovss[[1]]$`F value`[1]
      Pobs=aovss[[1]]$`Pr(>F)`[1]
      perm=permute::shuffleSet(sampn,rand)
      if (nrow(perm) < rand) {rand = nrow(perm)}
      eps=sqrt(.Machine$double.eps)
      aovi<-function(i,...)
      {
        Vi=V[perm[i,]]
        aovsi=summary(aov(Vi~grp))
        aovsi[[1]]$`F value`[1]
      }
      Frand=sapply(1:rand,aovi)
      Paovp=(sum(Frand>=(Fobs-eps))+1)/(rand+1)
      list(F.value=Fobs,P.permaov=Paovp)
    }
    
    for(i in 1:ncol(treat))
    {
      trt.levi=sort(unique(as.vector(treat[,i])))
      cgroupi=cgroup[[i]]
      for(j in 1:length(trt.levi))
      {
        trtij=trt.levi[j]
        idij=which(cgroupi==trtij)
        obsij=cdisjobs[idij,i]
        nullij=cdisj.rdm[idij,i]
        aovij=poneanova(V=c(obsij,nullij),grp=c(rep("obs",length(obsij)),rep("null",length(nullij))))
        dispsum[[k]]=c(colnames(treat)[i],trtij,mean(obsij),sd(obsij),mean(nullij),sd(nullij),unlist(aovij))
        k=k+1
      }
    }
    disp.sum=Reduce(rbind,dispsum)
    disp.sum=cbind(disp.sum,p.nulltest=cdis.p[match(paste0(disp.sum[,1],".",disp.sum[,2]),names(cdis.p))])
    rownames(disp.sum)=c()
    colnames(disp.sum)=c("treatment.type","treatment.id","Disp.distance.mean","Disp.distance.sd",
                         "Disp.distance.Null.mean","Disp.distance.Null.sd","F.value","permANOVA.p","NullModel.p")
    disp.sum=data.frame(disp.sum[,1:2],apply(disp.sum[,3:ncol(disp.sum)],2,as.numeric))
  }
  
  BC.rd=array(unlist(BC.rd),dim=c(nrow(BC.rd[[1]]),ncol(BC.rd[[1]]),length(BC.rd)))
  
  gc()
  
  # Step 4 # calculate RC, SES, and ST.Ratio
  # calculate RC value according to Chase et al 2011.
  comp<-function(x,c){(x<c)+0.5*(x==c)}
  alpha=matrix(rowSums(apply(BC.rd,3,comp,c=BC.obs)),nrow=nrow(BC.obs))/rand
  rc=(alpha-0.5)*2
  rownames(rc)=rownames(BC.obs)
  colnames(rc)=colnames(BC.obs)
  
  # calculate SES value according to betaNTI (Fine et al 2011)
  Gm=apply(BC.rd,c(1,2),mean)
  ses.res=(BC.obs-Gm)/(apply(BC.rd,c(1,2),sd))
  diag(ses.res)<-0
  ses.res[is.na(ses.res)]=0 # if standard deviation is zero, observed is not differentiable from null, SES=0.
  ses.res[is.na(BC.obs)]=NA # if observed BC not valid, SES is not applicable.
  colnames(ses.res)<-rownames(ses.res)<-samp.name<-rownames(BC.obs)
  
  # calculate ST (stochasticity ratio) modified from Zhou et al 2014.
  ST.Ratio.old=(Dmax-Gm)/(Dmax-BC.obs)
  ST.Ratio=ST.Ratio.old
  ST.Ratio[which(ST.Ratio.old>1)]=(Gm/BC.obs)[which(ST.Ratio.old>1)]
  ST.Ratio[which(is.nan(ST.Ratio.old))]=(Gm/BC.obs)[which(is.nan(ST.Ratio.old))] # NaN is caused by 0/0, which should be replaced by 1/1.
  diag(ST.Ratio)=NA
  
  # Step 5 # statistics of SES, RC, ST.Ratio in each treatment
  summl=list()
  k=1
  if(!is.null(treat))
  {
    for(i in 1:ncol(treat))
    {
      trt.levi=sort(unique(as.vector(treat[,i])))
      for(j in 1:length(trt.levi))
      {
        trtij=trt.levi[j]
        sampij=rownames(treat)[which(treat[,i]==trtij)]
        idij=which(samp.name %in% sampij)
        Dmij=mean(as.dist(BC.obs[idij,idij]))
        Cmij=Dmax-Dmij
        Dsdij=sd(as.dist(BC.obs[idij,idij]))
        
        Gmij=mean(as.dist(Gm[idij,idij]))
        Emij=Dmax-Gmij
        Gsdij=sd(as.dist(Gm[idij,idij]))
        
        rcij=as.vector(as.dist(rc[idij,idij]))
        rcmij=mean(rcij)
        rcsdij=sd(rcij)
        STratio.rcij=sum(abs(rcij)<0.95,na.rm=TRUE)/length(rcij)
        
        sesij=as.vector(as.dist(ses.res[idij,idij]))
        sesmij=mean(sesij)
        sessdij=sd(sesij)
        STratio.sesij=sum(abs(sesij)<1.96,na.rm=TRUE)/length(sesij)
        
        Cij=as.vector(as.dist(Dmax-BC.obs[idij,idij]))
        Eij=as.vector(as.dist(Dmax-Gm[idij,idij]))
        
        E.95l=quantile(Eij,probs = c(0.025,0.975))
        C.quart=quantile(Cij)
        E.quart=quantile(Eij)
        
        STrij=Eij/Cij
        STrij[which(STrij>1)]=((Dmax-Eij)/(Dmax-Cij))[which(STrij>1)]
        STrij[which(is.nan(STrij))]=((Dmax-Eij)/(Dmax-Cij))[which(is.nan(STrij))]
        STrmij=mean(STrij[!is.infinite(STrij)],na.rm=TRUE)# remove NA and inf
        STrsdij=sd(STrij[!is.infinite(STrij)],na.rm=TRUE)
        STr.quart=quantile(STrij,na.rm = TRUE)
        
        summl[[k]]=c(colnames(treat)[i],trtij,STrmij,STrsdij,STr.quart,
                     Dmij,Cmij,Dsdij,C.quart,Gmij,Emij,Gsdij,E.quart,
                     STratio.rcij,rcmij,rcsdij,STratio.sesij,sesmij,sessdij)
        k=k+1
      }
    }
    summ=Reduce(rbind,summl)
    rownames(summ)=c()
    colnames(summ)=c("treatment.type","treatment.id","StochasticityRatio.mean","StochasticityRatio.sd",
                     "StochasticityRatio.min","StochasticityRatio.0.25","StochasticityRatio.median","StochasticityRatio.0.75","StochasticityRatio.max",
                     "Dissimilarity.mean","Similarity.mean","Dis_Similarity.sd","Similarity.min","Similarity.0.25","Similarity.median","Similarity.0.75","Similarity.max",
                     "Dissimilarity.Null.mean","Similarity.Null.mean","Dis_Similarity.Null.sd","Similarity.Null.min","Similarity.Null.0.25","Similarity.Null.median","Similarity.Null.0.75","Similarity.Null.max",
                     "STratio.RC","RC.mean","RC.sd","STratio.SES","SESij.mean","SESij.sd")
    summ=data.frame(summ[,1:2],apply(summ[,3:(ncol(summ)-1)],2,as.numeric),SES.cohen.magnitude=summ[,ncol(summ)])
    summ.all=cbind(summ,disp.sum[,-(1:2)])
  }else{
    cdisjobs=NULL
    cdisj.rdm=NULL
    cd.summ=NULL
    summ.all=NULL
  }
  
  # Step 5 # significance test of RC, SES, and Stochasticity ratio between treatments.
  if(!is.null(treat))
  {
    # modified permutational multivariate ANOVA.
    panova.sm<-function(m,treat,perm.rand=999,ignore.diag=TRUE,trace=TRUE,nworker=1)
    {
      if(sum(rownames(treat)!=rownames(m))>0) stop("rownames of treat and tested matrix not match.")
      if(sum(rownames(treat)!=colnames(m))>0) stop("rownames of treat and colnames of tested matrix not match.")
      if(sum(is.infinite(m))>0)
      {
        m[which(m==Inf)]=max(c(1,m[which(!is.infinite(m))]))*10000
        m[which(m==-Inf)]=min(c(-1,m[which(!is.infinite(m))]))*10000
      }
      
      trt.lev=sort(unique(treat[,1]))
      sampn=nrow(m)
      k=1
      perms<-rands<-Rname<-groups<-list()
      
      perms[[k]]=rbind(1:sampn,as.matrix(permute::shuffleSet(sampn,perm.rand)))
      groups[[k]]=treat
      rands[[k]] = nrow(perms[[k]])
      Rname[[k]]=c("all.grp","all.grp")
      
      for(i in 1:(length(trt.lev)-1))
      {
        sampi=rownames(treat)[treat[,1]==trt.lev[i]]
        for(j in (i+1):length(trt.lev))
        {
          sampj=rownames(treat)[treat[,1]==trt.lev[j]]
          k=k+1
          sampij=c(sampi,sampj)
          perms[[k]]=rbind(1:length(sampij),as.matrix(permute::shuffleSet(length(sampij),perm.rand)))
          groups[[k]]=treat[match(sampij,rownames(treat)),,drop=FALSE]
          rands[[k]]=nrow(perms[[k]])
          Rname[[k]]=c(trt.lev[i],trt.lev[j])
        }
      }
      
      
      eps=.Machine$double.eps
      rand=max(unlist(rands))
      if(trace){trace.seq=seq(from=1,to=rand,by=200)}else{trace.seq=NULL}
      
      Frand<-function(u,perms,m,groups,Rname,ignore.diag,trace.seq,rand)
      {
        if(u %in% trace.seq) message("---Now PermAnova u=",u," in ",rand,".---",date())
        Fs<-Ps<-list()
        for(k in 1:length(perms))
        {
          if(nrow(perms[[k]])<u)
          {
            Fs[[k]]<-Ps[[k]]<-NA
          }else{
            grpk=groups[[k]][perms[[k]][u,],,drop=FALSE]
            rownames(grpk)=rownames(groups[[k]])
            grpk.lev=if(k==1){unique(grpk[,1])}else{Rname[[k]]}
            Vk=lapply(1:length(grpk.lev),
                      function(i)
                      {
                        sampi=rownames(grpk)[which(grpk[,1]==grpk.lev[i])]
                        idi=which(rownames(m) %in% sampi)
                        out=as.vector(as.dist(m[idi,idi]))
                        if(!ignore.diag) out=c(out,diag(m[idi,idi]))
                        out
                      })
            grpkv=lapply(1:length(Vk),function(i){rep(grpk.lev[i],length(Vk[[i]]))})
            aovs=aov(unlist(Vk)~unlist(grpkv))
            aovss=summary(aovs)
            Fs[[k]]=aovss[[1]]$`F value`[1]
            Ps[[k]]=aovss[[1]]$`Pr(>F)`[1]
          }
        }
        list(Fs=unlist(Fs),Ps=unlist(Ps))
      }
      
      
      if(nworker==1)
      {
        fps=lapply(1:rand,Frand,perms,m,groups,Rname,ignore.diag,trace.seq,rand)
      }else{
        c1<-parallel::makeCluster(nworker,type="PSOCK")
        if(trace) message("Now parallel computing PermAnova. begin at ", date(),". Please wait...")
        fps<-parallel::parLapply(c1,1:rand,Frand,perms,m,groups,Rname,ignore.diag,trace.seq,rand)
        parallel::stopCluster(c1)
      }
      #fps=lapply(1:rand,Frand,perms,m,groups,Rname,ignore.diag,trace.seq,rand)
      fm=sapply(1:length(fps),function(i){fps[[i]]$Fs})
      Fobs=fps[[1]]$Fs
      Pobs=fps[[1]]$Ps
      
      Paovp=(rowSums(fm>=matrix((Fobs-eps),nr=length(Fobs),nc=rand,byrow = FALSE),na.rm = TRUE))/(rowSums(!is.na(fm)))
      Rname=Reduce(rbind,Rname);rownames(Rname)=c();colnames(Rname)=c("group1","group2");Rname=data.frame(Rname)
      data.frame(Rname,F.obs=Fobs,P.Ftest=Pobs,P.permanova=Paovp)
    }
    
    test.res=list(RC=rc,SES=ses.res,STratio=ST.Ratio)
    perm.test=list();k=1
    for(i in 1:length(test.res))
    {
      for(j in 1:ncol(treat))
      {
        if(trace) message("---Now permanova i=",i," ",names(test.res)[i]," j=",j," treatment=",colnames(treat)[j],". ",date())
        out=panova.sm(m=test.res[[i]],treat = treat[,j,drop=FALSE],trace=trace,nworker = nworker,perm.rand = perm.rand)
        perm.test[[k]]=data.frame(Index=rep(names(test.res)[i],nrow(out)),treatment.type=colnames(treat)[j],out)
        k=k+1
      }
    }
    perm.test.out=Reduce(rbind,perm.test)
  }else{
    perm.test.out=NULL
  }
  rownames(BC.rd)=rownames(BC.obs);colnames(BC.rd)=colnames(BC.obs)
  if(trace){message("taxnull finished. ",date())}
  list(RC=rc,SES=ses.res,ST.Ratio=ST.Ratio,ST.Ratio.old=ST.Ratio.old,
       Disp.summary=cd.summ,Disp.dist.obs=cdisjobs,Disp.dist.null=cdisj.rdm,
       beta.obs=BC.obs,beta.rand=BC.rd,
       summary=summ.all,PERM.test=perm.test.out)
}

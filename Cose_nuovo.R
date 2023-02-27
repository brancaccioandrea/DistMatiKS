distance.matiks<-function(M1,M2){
  #Per il momento non c'è differenza tra MatiKS 9 e MatiKS 4
  #nel futuro quello sarà il primo controllo da fare
  ## per il momento la differenza tra il numero di celle è 0
  diff_num_cells<-abs(length(M2)-length(M1))
  diff_hrule <- rules.distance(M2$hrule,M2$hrule)
  diff_vrule <- rules.distance(M2$vrule,M2$vrule)

  
  for (variable in vector) {
    
  }
    
  
}

distance.field<-function(c1,c2){
  vis_1<-c1$visible
  vis_2<-c2$visible
  
  #Remove all the not visible fields
  if(any(vis_1==0))
  {
    c1_old<-c1
    variable<-names(c1)
    for(vars in 1:length(variable))
    {
      c1[[vars]]<- c1[[vars]][vis_1==1]
    }
  }
  
  #Remove all the not visible fields
  if(any(vis_2==0))
  {
    c2_old<-c2
    variable<-names(c2)
    for(vars in 1:length(variable))
    {
      c2[[vars]]<- c2[[vars]][vis_1==1]
    }
  }
  
  #Scorriamo i contenuti di ogni celle per trovare quale cella minimizza la distanza  
  token <- matrix(0, ncol=sum(vis_1), nrow = sum(vis_2))
  variable<-names(c2)
  Dist <- list()
  for(vars in 1:length(variable))
  {
    Dist[[variable[vars]]]<-token
    for(col1 in 1:sum(vis_1))
    {
      for(row2 in 1:sum(vis_2))
      {
        ##vanno messi dei controlli qui
        Dist[[vars]][row2,col1]<- 1-sum(c1[[vars]][[col1]]==c2[[vars]][[row2]])
      }
    }
  }
}


rules.distance<-function(h1,h2)
{
  n <- length(h1)
  m <- length(h2)

  h1 <- matrix(rep(h1,m),ncol=n)
  h2 <- matrix(rep(h2,n),byrow=TRUE,nrow=m)
  ##Just to check if they share all the rules or a portion 
  ##NB If the rules are repeated this won't work
  D <- max(m,n)-sum(h1==h2)
  return(D)
}

#Read Graph: 2012 - 2017
read_graph = function(){
  g = list()
  setwd('/home/robert/Downloads/Insurance/reinsurance_network_Ariah2/full_graph')
  for (i in 1:5){
    #read a graph
    #transfer to an igraph: g <- igraph::graph.lattice(c(5, 4))
    name_string = paste('reins_graph_full_201', i+1, '.gexf', sep = '')
    gg = read.gexf(name_string)
    gg2 = gexf.to.igraph(gg)
    g[[i]] = gg2
  }
  return (g)
}



#Function: choose graph
#First day indice from 2013 to 2017: 250/503/754/1006
graph_select = function(time){
  if (time < 250){
    return (g[[1]])
  }
  else if (time<503){
    return (g[[2]])
  }
  else if (time < 754){
    return (g[[3]])
  }
  else if (time < 1006){
    return (g[[4]])
  } 
  return (g[[5]])
} 

output_ggb = function(){
  for (i in 1:80){
    #setwd('/home/robert/Downloads/Insurance/cov_files')
    ss = read.csv(paste('/home/robert/Downloads/Insurance/cov_files_sample/',i-1,'.csv',sep = ''), header = T )
    ss2 = as.matrix(ss)
    g2 = graph_select(i)
    fit_global = ggb(ss2, g2, type = "global", nlam = 10)
    #fit_local <- ggb(ss2, g2, type = "local", nlam = 10)
    #fit_global$Sig[[i]]: i = 10 is less sparse; 1 most sparse
    write.csv(as.matrix(fit_global$Sig[[5]]), paste('/home/robert/Downloads/Insurance/cov_files_ggb/', i-1, '.csv', sep = ""), row.names=FALSE )
    print (i) 
  }  
}
#################################################
###########  Main() ################
library(ggb)
library(rgexf)
g = read_graph()
output_ggb()





###########################################################
#####################example
n <- 100
p <- 20
#g needs to be a igraph object
g <- igraph::graph.lattice(c(5, 4))
x <- matrix(rnorm(n*p), n, p)
S <- cov(x)
fit_global <- ggb(S, g, type = "global", nlam = 10)
fit_local <- ggb(S, g, type = "local", nlam = 10)

#20180316-2

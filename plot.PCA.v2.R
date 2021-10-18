require('combinat')
npc <-8

## set the plot arragnment here ##
layout.matrix <- matrix (c(1:npc^2),nrow= npc,ncol=npc,byrow = T)
layout.matrix
layout(mat = layout.matrix,
       heights = c(1, rep(3,npc-1)),
       widths = c(1, rep(3,npc-1))
)

#layout.show(npc^2)


### Example dataset
C <- as.matrix(read.table("~/Downloads/baltic.pca.cov"))
e <- eigen(C)
t<-read.table("~/Downloads/baltic.rmLow.info.csv",sep=",",header=T)
t$color <- as.numeric(t$pops)
pcs <- combn(npc,2)

var.exp <-e$values[1:npc]/sum(e$values)

###

valid_c = 1
skip_n <- c( 1,sort(layout.matrix[-1,-1][lower.tri(layout.matrix[-1,-1])]) )
row_h <- layout.matrix[1,][-1] # column names
col_h <- layout.matrix[,1][-1]

for (i in 1:max(layout.matrix)) {
    if (!i %in% skip_n) {
    
      if (i %in% row_h) { # first row
        par(mar=c(0.5,0.5,0.5,0))
        j = i-1
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(0.5,0.3,paste0("PC",j," : ",round(var.exp[j] *100,1), "%"), 
             cex = 1.5)
      } else if( i %in% col_h) { # first column
        par(mar=c(0.5,0.5,0.5,0))
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        k = round(i / npc)+1
        text(0.3,0.7, paste0("PC",k,'\n',round(var.exp[k] *100,1), "%"), 
             cex = 1.5)
      } else {
        par(mar=c(2,1.5,0.5,1))
        pcx=pcs[1,valid_c]
        pcy=pcs[2,valid_c]
        plot(e$vectors[,c(pcx,pcy)],col=t$color,cex=1.5,cex.lab=1.5,cex.axis=1.5,xlab="",ylab="",pch=16)
        valid_c = valid_c + 1
        
      }
      
    } else {
      
      if ( i == (npc+1)*2 ) { # legend
        par(mar=c(2,1.5,0.5,1))
        plot(1, type = "n", axes=FALSE, xlab="", ylab="")
        legend(x="center", inset=0,
               col= unique(t$color),
               legend=as.character(unique(t$pops)),
               bty = 'n',pch=16,
               cex = 1)
      } else { 
      plot(0,type='n',axes=FALSE,ann=FALSE)
      }
    }
}


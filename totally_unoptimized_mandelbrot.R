options(warn=-1) #Suppress the warning coming from the color pacakge
library(scico)

lajolla <- scico::scico(250, palette= 'lajolla') #picked a color scheme
color <- c(lajolla)

# A fractal function
mandelbrot <<- function(i,j){
  
  bound <- 1 #flag/swtich
  x <- i-460 #
  y <- 240-j

  a <- complex(real = x/200, imaginary = y/200) #real numbers
  b <- complex(real=0, imaginary = 0) #imaginary numbers

  for(k in seq(length(color))){
    # I am doing with a b^2+ba sequence to make fracatl rings
    b <- b*b+a 
    if(Mod(b)>2){
      color <- k
      bound <- 0
      break;
    }
  }
  
  return(list(flag= bound, c= color))
}

png(filename="mandelbrot_bad.png", bg= "black",height = 600, width = 600, units = "px") 

#plot attributes
plot(c(50, 600), c(0,500), type = 'l', xaxt='n',yaxt='n', ann=FALSE)

# iterations
for(i in 1:1920){ 
  for(j in 1:1080){
    f_x= mandelbrot(i,j) 
    
    if(f_x$flag){ #if false, do nothing
      points(i,j,pch=46) #line graph
    }
    else{
      points(i,j,pch=46,col=color[f_x$c]) #color line if flag is True
    }
  }
}

dev.off()
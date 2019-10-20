p <- treemap(ruiz,
             index=c("grupo", "Item"),
             vSize="MillonesUS",
             type="index",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )  
) 

d <- d3tree3(p ,  rootname = "(Millones de USD)" )
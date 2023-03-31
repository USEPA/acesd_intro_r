render_all<-function(path=".",pattern=c("*.Rmd", "*.qmd"), ignore = NULL){
  files <- NULL
  for(i in pattern){
    files <- c(files, list.files(path,i,full.names = T))
  }
  if(!is.null(ignore)){
    
    files <- files[!grepl(ignore, files)]
  }
  
  for(i in files){
    if(grepl(".qmd", i)){
      quarto::quarto_render(i)
    } else {
      out <- stringr::str_replace(i,"Rmd","md")
      if(!file.exists(out)){
        knitr::knit(i,output=out)
      } else if((file.info(i)$mtime-file.info(out)$mtime)>0){
        knitr::knit(i,output=out)
      }
    }
  }
  
  system("git add -A")
  system('git commit -m "new render"')
  system("git push origin main")
}
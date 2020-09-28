tax_res<- function(x){require(taxize)
  a<-gnr_resolve((as.character(x)), 
                 resolve_once = TRUE, 
                 best_match_only = TRUE, 
                 with_context = TRUE, 
                 canonical = TRUE)
    logic<- a$submitted_name==a$matched_name2
  b<- return(data.frame(a, logic))
}
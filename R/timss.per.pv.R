timss.per.pv <- 
  function(pvlabel, by, per, data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.per.pv(pvnames = pvlabel, by=by, per=per, data=data, export=export, 
                  name= name, folder=folder, config=timss8_conf)

}


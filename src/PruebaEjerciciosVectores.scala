

object PruebaEjerciciosVectores {
  
    def llenar(n:Int): Array[Double]={
      
    var num=new Array[Double](n)
    num(0)=0.0
    for(i <- 0 until num.length){
      println("Ingrese el valor  "+ i + ": ")
       num(i)= readDouble()
    }
    num
  }
  
  def imprimir(vector:Array[Double]):Unit={
    for(i <- 0 until vector.length){
      print(" "+vector(i)+" ")
    }
    println()
  }
  
  def ordenar(v:Array[Double], n:Int):Array[Double]={
    var t=0.0
     for(i <- 0 to n-1){
         for(x <-0 to n-1){
             if(v(i) < v(x)){
               t=v(i)
               v(i)=v(x)
               v(x)=t
             }
         }
     }
     v
  }
  
  
  def media(vector:Array[Double]): Double ={
    var media=0.0
    var sum = 0.0
    for(i <- 0 until vector.length){
      sum = sum +vector(i)
    }
    media = sum /vector.length
    media
  }
  
  
  
  def moda(v:Array[Double]): Double ={
    var m = 0.0
    var moda = 0.0
    
    for(i <- 0 until v.length){
      var rep = 0.0
      
      for(j <- 0 until v.length){
        if(v(i)==v(j))
          rep+=1
          
      if(rep>m){
        moda = v(i)
        m = rep
    }
  }
    }
    moda
  }
  
  def mediana(v:Array[Double]): Double ={  
    var medna=0.0
    
    if(v.length%2==0)
      medna = ((v((v.length-1)/2)) + (v(((v.length-1)/2)+1)))/2
    else{
      medna = v(((v.length)/2).toInt)
    }
    medna
  }
  
  def desviacionRespectoMedia(v:Array[Double]): Double ={ 
    var med= media(v)
    var mod = moda(v)
    var drm = mod - med
    drm
  }
  
  
  def desviacionMedia(v:Array[Double]): Double ={ 
    var med = media(v)
    var sum =0.0
   for(i <- 0 until v.length)
      sum = sum +(v(i)-med)
    
    var dm = sum /v.length
    dm
  }
  
  def varianza(vector:Array[Double]): Double ={ 
   var med = media(vector)
    var sum = 0.0
   for(i <- 0 until vector.length)
      sum = sum+( (vector(i)-med)* (vector(i)-med) )
    
    var va= sum/vector.length
    va
  }
  
  
  def desviacionEstandar(v:Array[Double]): Double ={ 
    var va = varianza(v)
    val desviacionEstandar=math.sqrt(va)
    desviacionEstandar
  }

  def main(args: Array[String]): Unit = {
    println("Ingrese el tamaño del vector: ")
    var n=readInt()
    var v = llenar(n)  
    
    var menu=10
    
    while(menu>0){
      println("Seleccione una opcion")
      println("1 Imprimir vector")
      println("2 Ordenar vector")
      println("3 Calcular media")
      println("4 Calcular moda")
      println("5 Calcular mediana")
      println("6 Desviación respecto de la media")
      println("7 Desviación media")
      println("8 Varianza")
      println("9 Desviación estándar")
      println("0 Salir")
      menu=readInt()
      
      if(menu==1){
        imprimir(v)
      }
     
     if(menu==2){
       v= ordenar(v,n)
       imprimir(v)
     }
          
     if(menu==3){
       val med= media(v)
       println("Media: "+med)
     }
     
     if(menu ==4){
       val mod = moda(v)
       println("Moda: "+mod)
     }
     
     if(menu ==5){
       v = ordenar(v,n)
       val medna = mediana(v)
       println("Mediana: "+medna)
     }
     
     if(menu ==6){
       val drm = desviacionRespectoMedia(v)
       println("Desviacion respecto de la media: "+drm)
     }
     
     if(menu ==7){
       val dm=desviacionMedia(v)
       println("Desviacion media: "+dm)
     }
     
     if(menu ==8){
       val va = varianza(v)
       println("Varianza: "+ va)
     }
      
     if(menu ==9){
       val desviacion = desviacionEstandar(v)
       println("Desviacion estandar: "+desviacion)
     }
    }
    
  }
}
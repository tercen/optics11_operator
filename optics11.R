 
do.optics11 = function(data, label){
  
  loads = data[[label]]
  indentation = data$.y
  time = data$.x
   
  fs = 1/mean(diff(time))
  
  loads = loads - mean(loads)
   
  df = data.frame(time=time,loads=loads,indentation=indentation)
  
  dff = df %>% filter(loads > 0.3 , time > 10 , time < max(time) - 5 )
    
  ################## 
 
  loads.smooth = kernapply( dff$loads, kernel("modified.daniell", c(9,9,9)))
  loads.smooth = dff$loads
    
  peakhits = peakPick::peakpick(matrix(loads.smooth , ncol=1), 100, peak.npos=10)
  
  peak.index = which(peakhits == TRUE)
    
  peaks.df = data.frame(time=dff$time[peak.index], load= dff$load[peak.index])
   
  cluster.df = data.frame(time=peaks.df$time)
  
  plot(cluster.df)
   
  nc = NbClust(cluster.df, min.nc=2, max.nc=6, method="centroid", index='hartigan')
  
  print(nc)
  
  fit.km = kmeans(cluster.df, nc$Best.n[1], nstart=25)
  
  peaks.df = cbind(peaks.df, cluster=fit.km$cluster)
   
  region.df = peaks.df %>%
    group_by(cluster) %>%
    summarise(min.time=min(time), max.time=max(time)) 
   
  ################## 
   
  do.fft = function(data,fs){
    norm.data = data - mean(data)
    fft.data = fft(norm.data)
    fft.data.mod = Mod(fft.data[1:(length(fft.data)/2)])
    
    max.mod = max(fft.data.mod)
    max.mod.index = which(fft.data.mod == max.mod)
    
    freq  =  (max.mod.index-1) * fs / length(fft.data)
    phase  = Arg(fft.data[max.mod.index])
    amplitude = max.mod
    
    return(list(freq=freq, phase=phase, amplitude=amplitude))
  }
  
  peakFreq = function(region.df, df){
    
    fs = 1 / mean(diff(df$time))
    region = df %>% filter(time > region.df$min.time[1] , time < region.df$max.time[1] )
    
    fft.load = do.fft(region$loads, fs)
    fft.indentation = do.fft(region$indentation, fs)
    
    ratio =  fft.load$amplitude / fft.indentation$amplitude
    phase.shift = fft.load$phase - fft.indentation$phase 

    storage = ratio * cos(phase.shift)
    loss = ratio * sin(phase.shift)
    tan.delta = loss / storage
    
    return(tibble(.ci= data$.ci[1],
                  .ri= data$.ri[1],
                  freq=fft.load$freq,
                  freq.log=log(fft.load$freq),
                  storage=storage,
                  loss=loss,
                  ratio=ratio,
                  tan.delta=tan.delta,
                  phase.shift=phase.shift,
                  phase.load=fft.load$phase,
                  amplitude.load=fft.load$amplitude,
                  phase.indentation=fft.indentation$phase,
                  amplitude.indentation=fft.indentation$amplitude))
  }
  
  load.freq = region.df %>%
    group_by(cluster) %>%
    do(peakFreq(.,df)) 
   
  return (load.freq)
   
}
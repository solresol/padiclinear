> module WordNeighbourhood where
> import NearestNeighbours
> import WordEncoding
> import Metric
>
>
> wordNeighbourhood :: Codec a => Metric -> NearnessMeasure -> a -> NeighbourhoodFinder String String
> wordNeighbourhood m  nm codec = NeighbourhoodFinder wordNeighbourhood'
>   where
>    wordNeighbourhood' vocab searchword = ratpoints
>     where corpus = [(word2number codec t1, word2number codec t2)| (t1,t2) <- vocab]
>           centre = word2number codec searchword
>           l = nearestPoints corpus  m nm centre
>           ratpoints = [(number2word codec x, number2word codec y) | (x,y,_) <- l]
> 
> padicWordNeighbourhood nm codec = wordNeighbourhood (Padic 2) nm codec
> euclideanWordNeighbourhood nm codec = wordNeighbourhood Euclidean nm codec
>
> 

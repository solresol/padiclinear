COMMON_OBJS=Scoring.lhs PadicLinguistic.lhs Metric.lhs PadicExpansion.lhs RationalGeometry.lhs PadicLinear.lhs PadicExpansion.lhs NearestNeighbours.lhs WordEncoding.lhs MachineLearning.lhs WordNeighbourhood.lhs Siegel.lhs LocalLearning.lhs EuclideanLinguistics.lhs CsvReader.lhs VocabEnumerationWordEncoding.lhs Primes.lhs Scoring.lhs UnicodeWordEncoding.lhs

unicode2num: unicode2num.lhs $(COMMON_OBJS)
	ghc -o unicode2num unicode2num.lhs

singular2plural: singular2plural.lhs $(COMMON_OBJS)
	ghc -o singular2plural singular2plural.lhs




f_wordcloud <- function(vec,tcorrectif,png_file=NULL) {

    docs <- Corpus(VectorSource(vec))
    ##inspect(docs)

    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")

                                        # Convertir le texte en minuscule
    docs <- tm_map(docs, content_transformer(tolower))
                                        # Supprimer les nombres
    docs <- tm_map(docs, removeNumbers)
                                        # Supprimer les mots vides français
    docs <- tm_map(docs, removeWords, stopwords("french"))
                                        # Supprimer votre propre liste de mots non désirés
    docs <- tm_map(docs, removeWords, c("le","la","les","l'","de","des","d'","du","un","une","se","ses","ces","ce","ça","sa","S'","c","cette","est","sont","es","et","ont","eu","fait","a","à","m'","n'","pour","mais","ou","où","dans","avec","au","aux","dont","par","que","qui","qu'","tout","tous","toute","toutes","en","sur","sous","sans","entre","ni","ne","non","pas","plus","autre","même","aucun","personne","rien","s'","il","elle","ils","elles","je","nous","on","pô","mettre","toute","souhaite","sais","heu","fait","permet","wiki","\r","\n","=","indicateur","permet","permettant","aussi","trop","indicateurs","agent","double","délinquant","police","mêmes","comme","réponse","essentiellement","utilise","lors","plutôt","faible","très","particulièrement","approche","fuis","peste","degré","modeste","choisis","moins","possible","préfère","selon","activité","autour","concentrent","directement","liés","loin","obtenus","pin","procesionnaire","près","certains","quelque","avoir","élément","sein","vue","idiote","faire","cretins","font","notamment","blagues","mise","bienvenue","bêtise","celles","contre","faut","sein","mis","situe","arrive","top","déjà","idiotes","peux","autres","trouver","perdre","attentes","bien","sûre","visio","ainsi","faire","petite","luc","sujet","thème","finir","finis","crétins","questionnaire","être","peut","grande","écrit","jusqu","mieux","bêtises","définition","lundi","vois","travers","etc","inra","large","travaux","stage","via","floue","face"))
                                        # Supprimer les ponctuations
    docs <- tm_map(docs, removePunctuation)
                                        # Supprimer les espaces vides supplémentaires
    docs <- tm_map(docs, stripWhitespace)
                                        # Text stemming
                                        # docs <- tm_map(docs, stemDocument)


    ## correctifs

    inspect(docs)
    for(i in 1:nrow(tcorrectif)){
        w1 <- paste0(tcorrectif[i,search]," ")
        w2 <- paste0(tcorrectif[i,replaceBy]," ")
        docs <-tm_map(docs, content_transformer(function(x) gsub(x, pattern = w1 , replacement = w2)))
###        docs <- gsub(tcorrectif[i,search],tcorrectif[i,replaceBy],docs)
    }

    setDT(tcorrectif)
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)

    set.seed(1234)

    if(!is.null(png_file)){
        png(png_file)
        wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                  max.words=200, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"))
        dev.off()
    }

    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))

    return(d)

}




f_wordcloud_simple <- function(vec,png_file=NULL) {

    docs <- Corpus(VectorSource(vec))
    ##inspect(docs)


    inspect(docs)
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)

    set.seed(1234)

    if(!is.null(png_file)){
        png(png_file)
        wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                  max.words=200, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"))
        dev.off()
    }

    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))

    return(d)

}


Imports System.IO
Imports System.Text.Json
Imports System.Text.RegularExpressions

Public Class WordEmbeddings
    Public vocabulary As HashSet(Of String)
    Private wordToIndex As Dictionary(Of String, Integer)
    Private indexToWord As Dictionary(Of Integer, String)
    Private embeddingMatrix As Double(,)
    Private embeddingSize As Integer
    Private learningRate As Double
    Private windowSize As Integer
    Public cooccurrenceMatrix As Dictionary(Of String, Dictionary(Of String, Double))
    Private vectorSize As Integer
    Private iterations As Integer
    ''' <summary>
    ''' Initializes a new instance of the WordEmbeddings class.
    ''' </summary>
    ''' <param name="embeddingSize">The size of the word embeddings.</param>
    ''' <param name="learningRate">The learning rate for training the model.</param>
    ''' <param name="windowSize">The size of the context window.</param>
    Public Sub New(embeddingSize As Integer, learningRate As Double, windowSize As Integer)
        Me.embeddingSize = embeddingSize
        Me.learningRate = learningRate
        Me.windowSize = windowSize

        vocabulary = New HashSet(Of String)()
        wordToIndex = New Dictionary(Of String, Integer)()
        indexToWord = New Dictionary(Of Integer, String)()
    End Sub

    ''' <summary>
    ''' Calculates the Pointwise Mutual Information (PMI) matrix for the trained model.
    ''' </summary>
    ''' <returns>A dictionary representing the PMI matrix.</returns>
    Public Function CalculatePMI() As Dictionary(Of String, Dictionary(Of String, Double))
        Dim pmiMatrix As New Dictionary(Of String, Dictionary(Of String, Double))

        Dim totalCooccurrences As Double = GetTotalCooccurrences()

        For Each targetWord In cooccurrenceMatrix.Keys
            Dim targetOccurrences As Double = GetTotalOccurrences(targetWord)

            If Not pmiMatrix.ContainsKey(targetWord) Then
                pmiMatrix(targetWord) = New Dictionary(Of String, Double)
            End If

            For Each contextWord In cooccurrenceMatrix(targetWord).Keys
                Dim contextOccurrences As Double = GetTotalOccurrences(contextWord)
                Dim cooccurrences As Double = cooccurrenceMatrix(targetWord)(contextWord)

                Dim pmiValue As Double = Math.Log((cooccurrences * totalCooccurrences) / (targetOccurrences * contextOccurrences))
                pmiMatrix(targetWord)(contextWord) = pmiValue
            Next
        Next

        Return pmiMatrix
    End Function
    Private Function GetTotalCooccurrences() As Double
        Dim total As Double = 0

        For Each targetWord In cooccurrenceMatrix.Keys
            For Each cooccurrenceValue In cooccurrenceMatrix(targetWord).Values
                total += cooccurrenceValue
            Next
        Next

        Return total
    End Function

    Private Function GetTotalOccurrences(word As String) As Double
        Dim total As Double = 0

        If cooccurrenceMatrix.ContainsKey(word) Then
            total = cooccurrenceMatrix(word).Values.Sum()
        End If

        Return total
    End Function

    Public Function ExportModel() As WordEmbeddings
        Return Me
    End Function
    Public Shared Sub Main()
        ' Create an instance of the WordEmbeddings model
        Dim model As New WordEmbeddings(embeddingSize:=100, learningRate:=0.01, windowSize:=5)

        ' Define the corpus
        Dim corpus As String() = {
            "united states is a country.", "England is a country",
            "the united kingdom is in europe.",
            "united airlines is an airline company.",
            "doberman is a breed of dog.",
            "dogs have many breeds.",
            "dogs love eating pizza."
        }

        ' Train the model using the corpus
        model.Train(corpus)

        ' Discover collocations
        Dim words As String() = {"united", "states", "kingdom", "airlines"}
        Dim collocations As List(Of Tuple(Of String, String)) = model.DiscoverCollocations(words, threshold:=1)

        ' Display the discovered collocations
        Console.WriteLine("Collocations:")
        For Each collocation In collocations
            Console.WriteLine($"{collocation.Item1} {collocation.Item2}")
        Next

        ' Get the most similar words to "dog"
        Dim similarWords As List(Of String) = model.GetMostSimilarWords("dog", topK:=3)

        ' Display the most similar words
        Console.WriteLine("Most similar words to 'dog':")
        For Each word As String In similarWords
            Console.WriteLine(word)
        Next

        model.Train_GloveEmbeddings(corpus, 100, 100)
        ' Generate training data using the exported model
        Dim trainingDataGenerator As New WordEmbeddings.GenTrainingData(model)
        Dim trainingData As List(Of List(Of Integer)) = trainingDataGenerator.GenerateTrainingData(New List(Of String)(corpus))

        ' Get context indices and vectors for a target index
        Dim targetIndex As Integer = model.wordToIndex("united")
        Dim contextIndices As List(Of Integer) = trainingDataGenerator.GetContextIndices(trainingData(0), targetIndex)
        Dim contextVectors As List(Of Double()) = trainingDataGenerator.GetContextVectors(contextIndices)

        ' Display the context indices and vectors
        Console.WriteLine("Context indices:")
        For Each index As Integer In contextIndices
            Console.WriteLine(index)
        Next

        Console.WriteLine("Context vectors:")
        For Each vector As Double() In contextVectors
            For Each value As Double In vector
                Console.Write(value & " ")
            Next
            Console.WriteLine()
        Next

        ' Wait for user input to exit
        Console.ReadLine()
        ' Wait for user input to exit
        Console.ReadLine()
    End Sub
    Public Sub Train_GloveEmbeddings(corpus As String(), vectorSize As Integer, iterations As Integer)
        Me.vectorSize = vectorSize
        Me.iterations = iterations
        cooccurrenceMatrix = GenerateCooccurrenceMatrix(corpus, windowSize)
        InitializeEmbeddings()

        For iteration As Integer = 1 To iterations
            For Each targetWord In cooccurrenceMatrix.Keys
                If wordToIndex.ContainsKey(targetWord) Then
                    Dim targetIndex As Integer = wordToIndex(targetWord)
                    Dim targetEmbedding As Double() = GetEmbedding(targetIndex)

                    ' Initialize gradient accumulator for target embedding
                    Dim gradTarget As Double() = New Double(embeddingSize - 1) {}

                    For Each contextWord In cooccurrenceMatrix(targetWord).Keys
                        If wordToIndex.ContainsKey(contextWord) Then
                            Dim contextIndex As Integer = wordToIndex(contextWord)
                            Dim contextEmbedding As Double() = GetEmbedding(contextIndex)
                            Dim cooccurrenceValue As Double = cooccurrenceMatrix(targetWord)(contextWord)
                            Dim weight As Double = Math.Log(cooccurrenceValue)

                            ' Initialize gradient accumulator for context embedding
                            Dim gradContext As Double() = New Double(embeddingSize - 1) {}

                            ' Calculate the gradients
                            For i As Integer = 0 To embeddingSize - 1
                                Dim gradCoefficient As Double = weight * targetEmbedding(i)

                                gradTarget(i) = learningRate * gradCoefficient
                                gradContext(i) = learningRate * gradCoefficient
                            Next

                            ' Update the target and context embeddings
                            For i As Integer = 0 To embeddingSize - 1
                                targetEmbedding(i) += gradTarget(i)
                                contextEmbedding(i) += gradContext(i)
                            Next
                        End If
                    Next
                End If
            Next
        Next
    End Sub
    ''' <summary>
    ''' Trains the WordEmbeddings model using the provided corpus.
    ''' </summary>
    ''' <param name="corpus">The corpus of text used for training.</param>
    Public Sub Train(corpus As String())
        BuildVocabulary(corpus)
        InitializeEmbeddings()
        cooccurrenceMatrix = GenerateCooccurrenceMatrix(corpus, windowSize)

        For iteration As Integer = 1 To iterations
            For Each targetWord In cooccurrenceMatrix.Keys
                Dim targetIndex As Integer = GetOrCreateWordIndex(targetWord)
                Dim targetEmbedding As Double() = GetEmbedding(targetIndex)

                For Each contextWord In cooccurrenceMatrix(targetWord).Keys
                    Dim contextIndex As Integer = GetOrCreateWordIndex(contextWord)
                    Dim contextEmbedding As Double() = GetEmbedding(contextIndex)
                    Dim cooccurrenceValue As Double = cooccurrenceMatrix(targetWord)(contextWord)
                    Dim weight As Double = Math.Log(cooccurrenceValue)

                    For i As Integer = 0 To embeddingSize - 1
                        targetEmbedding(i) += weight * contextEmbedding(i)
                    Next
                Next
            Next
        Next
    End Sub
    ''' <summary>
    ''' Gets the most similar words to the specified word.
    ''' </summary>
    ''' <param name="word">The target word.</param>
    ''' <param name="topK">The number of similar words to retrieve.</param>
    ''' <returns>A list of the most similar words.</returns>
    Public Function GetMostSimilarWords(word As String, topK As Integer) As List(Of String)
        Dim wordIndex As Integer = wordToIndex(word)

        Dim similarities As New Dictionary(Of String, Double)()
        For Each otherWord As String In vocabulary
            If otherWord <> word Then
                Dim otherWordIndex As Integer = wordToIndex(otherWord)
                Dim similarity As Double = CalculateSimilarity(GetEmbedding(wordIndex), GetEmbedding(otherWordIndex))
                similarities.Add(otherWord, similarity)
            End If
        Next

        Dim orderedSimilarities = similarities.OrderByDescending(Function(x) x.Value)
        Dim mostSimilarWords As New List(Of String)()

        Dim count As Integer = 0
        For Each pair In orderedSimilarities
            mostSimilarWords.Add(pair.Key)
            count += 1
            If count >= topK Then
                Exit For
            End If
        Next

        Return mostSimilarWords
    End Function
    ''' <summary>
    ''' Discovers collocations among the specified words based on the trained model.
    ''' </summary>
    ''' <param name="words">The words to discover collocations for.</param>
    ''' <param name="threshold">The similarity threshold for collocation discovery.</param>
    ''' <returns>A list of collocations (word pairs) that meet the threshold.</returns>
    Public Function DiscoverCollocations(words As String(), threshold As Double) As List(Of Tuple(Of String, String))
        Dim collocations As New List(Of Tuple(Of String, String))

        For i As Integer = 0 To words.Length - 2
            For j As Integer = i + 1 To words.Length - 1
                Dim word1 As String = words(i)
                Dim word2 As String = words(j)

                If vocabulary.Contains(word1) AndAlso vocabulary.Contains(word2) Then
                    Dim vector1 As Double() = GetEmbedding(wordToIndex(word1))
                    Dim vector2 As Double() = GetEmbedding(wordToIndex(word2))
                    Dim similarity As Double = CalculateSimilarity(vector1, vector2)

                    If similarity >= threshold Then
                        collocations.Add(Tuple.Create(word1, word2))
                    End If
                End If
            Next
        Next

        Return collocations
    End Function

    Private Sub BuildVocabulary(corpus As String())
        Dim index As Integer = 0
        For Each sentence As String In corpus
            Dim cleanedText As String = Regex.Replace(sentence, "[^\w\s]", "").ToLower()
            Dim tokens As String() = cleanedText.Split()
            For Each token As String In tokens
                If Not vocabulary.Contains(token) Then
                    vocabulary.Add(token)
                    wordToIndex.Add(token, index)
                    indexToWord.Add(index, token)
                    index += 1
                End If
            Next
        Next
    End Sub
    Private Sub InitializeEmbeddings()
        Dim vocabSize As Integer = vocabulary.Count
        embeddingMatrix = New Double(vocabSize - 1, embeddingSize - 1) {}

        Dim random As New Random()
        For i As Integer = 0 To vocabSize - 1
            For j As Integer = 0 To embeddingSize - 1
                embeddingMatrix(i, j) = random.NextDouble()
            Next
        Next
    End Sub

    Private Function GenerateCooccurrenceMatrix(corpus As String(), windowSize As Integer) As Dictionary(Of String, Dictionary(Of String, Double))
        Dim matrix As New Dictionary(Of String, Dictionary(Of String, Double))

        For Each sentence In corpus
            Dim words As String() = sentence.Split(" "c)
            Dim length As Integer = words.Length

            For i As Integer = 0 To length - 1
                Dim targetWord As String = words(i)

                If Not matrix.ContainsKey(targetWord) Then
                    matrix(targetWord) = New Dictionary(Of String, Double)
                End If

                For j As Integer = Math.Max(0, i - windowSize) To Math.Min(length - 1, i + windowSize)
                    If i = j Then
                        Continue For
                    End If

                    Dim contextWord As String = words(j)
                    Dim distance As Double = 1 / Math.Abs(i - j)

                    If matrix(targetWord).ContainsKey(contextWord) Then
                        matrix(targetWord)(contextWord) += distance
                    Else
                        matrix(targetWord)(contextWord) = distance
                    End If
                Next
            Next
        Next

        Return matrix
    End Function
    Public Function CalculateCosineSimilarity(ByVal vector1 As Double(), ByVal vector2 As Double()) As Double
        Dim dotProduct As Double = 0
        Dim norm1 As Double = 0
        Dim norm2 As Double = 0

        For i As Integer = 0 To vector1.Length - 1
            dotProduct += vector1(i) * vector2(i)
            norm1 += vector1(i) * vector1(i)
            norm2 += vector2(i) * vector2(i)
        Next

        If norm1 <> 0 AndAlso norm2 <> 0 Then
            Return dotProduct / (Math.Sqrt(norm1) * Math.Sqrt(norm2))
        Else
            Return 0
        End If
    End Function
    Private Function GetEmbedding(index As Integer) As Double()
        If indexToWord.ContainsKey(index) Then
            Dim vector(embeddingSize - 1) As Double
            For i As Integer = 0 To embeddingSize - 1
                vector(i) = embeddingMatrix(index, i)
            Next
            Return vector
        Else
            Return Nothing
        End If
    End Function

    Private Function CalculateSimilarity(vectorA As Double(), vectorB As Double()) As Double
        Dim dotProduct As Double = 0
        Dim magnitudeA As Double = 0
        Dim magnitudeB As Double = 0

        For i As Integer = 0 To vectorA.Length - 1
            dotProduct += vectorA(i) * vectorB(i)
            magnitudeA += vectorA(i) * vectorA(i)
            magnitudeB += vectorB(i) * vectorB(i)
        Next

        If magnitudeA <> 0 AndAlso magnitudeB <> 0 Then
            Return dotProduct / (Math.Sqrt(magnitudeA) * Math.Sqrt(magnitudeB))
        Else
            Return 0
        End If
    End Function
    Public Class GenTrainingData
        Private model As WordEmbeddings

        Public Sub New(ByRef nModel As WordEmbeddings)
            model = nModel
        End Sub
        Public Function GenerateTrainingData(corpus As List(Of String)) As List(Of List(Of Integer))
            Dim trainingData As New List(Of List(Of Integer))()

            For Each sentence As String In corpus
                Dim cleanedText As String = Regex.Replace(sentence, "[^\w\s]", "").ToLower()
                Dim tokens As String() = cleanedText.Split()
                Dim sentenceIndices As New List(Of Integer)()

                For Each token As String In tokens
                    sentenceIndices.Add(Me.model.wordToIndex(token))
                Next

                trainingData.Add(sentenceIndices)
            Next

            Return trainingData
        End Function
        Public Function GetContextIndices(sentenceIndices As List(Of Integer), targetIndex As Integer) As List(Of Integer)
            Dim contextIndices As New List(Of Integer)()

            Dim startIndex As Integer = Math.Max(0, targetIndex - model.windowSize)
            Dim endIndex As Integer = Math.Min(sentenceIndices.Count - 1, targetIndex + model.windowSize)

            For i As Integer = startIndex To endIndex
                If i <> targetIndex Then
                    contextIndices.Add(sentenceIndices(i))
                End If
            Next

            Return contextIndices
        End Function
        Public Function GetContextVectors(contextIndices As List(Of Integer)) As List(Of Double())
            Dim contextVectors As New List(Of Double())

            For Each contextIndex As Integer In contextIndices
                Dim vector(model.embeddingSize - 1) As Double
                For i As Integer = 0 To model.embeddingSize - 1
                    vector(i) = model.embeddingMatrix(contextIndex, i)
                Next
                contextVectors.Add(vector)
            Next

            Return contextVectors
        End Function
    End Class
    Public Sub SaveModel(filePath As String)
        Dim modelData As New ModelData() With {
            .Vocabulary = vocabulary.ToList(),
            .EmbeddingMatrix = embeddingMatrix
        }

        Dim jsonString = JsonSerializer.Serialize(modelData)
        File.WriteAllText(filePath, jsonString)
    End Sub
    Private Function GetOrCreateWordIndex(word As String) As Integer
        If wordToIndex.ContainsKey(word) Then
            Return wordToIndex(word)
        Else
            Dim newIndex As Integer = vocabulary.Count
            vocabulary.Add(word)
            wordToIndex.Add(word, newIndex)
            indexToWord.Add(newIndex, word)
            Return newIndex
        End If
    End Function
    Public Shared Function LoadModel(filePath As String) As WordEmbeddings
        Dim jsonString = File.ReadAllText(filePath)
        Dim modelData As ModelData = JsonSerializer.Deserialize(Of ModelData)(jsonString)

        Dim model As New WordEmbeddings(embeddingSize:=modelData.EmbeddingMatrix.GetLength(1), learningRate:=0, windowSize:=0)
        model.vocabulary = New HashSet(Of String)(modelData.Vocabulary)
        model.embeddingMatrix = modelData.EmbeddingMatrix

        ' Reconstruct wordToIndex and indexToWord dictionaries
        For i As Integer = 0 To model.vocabulary.Count - 1
            model.wordToIndex(modelData.Vocabulary(i)) = i
            model.indexToWord(i) = modelData.Vocabulary(i)
        Next

        Return model
    End Function
    Private Class ModelData
        Public Property Vocabulary As List(Of String)
        Public Property EmbeddingMatrix As Double(,)
    End Class
End Class



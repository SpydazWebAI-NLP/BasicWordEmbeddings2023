Namespace iWordEmbeddings

    ''' <summary>
    '''Skip-gram with Negative Sampling:
    ''' Pros:
    ''' More computationally efficient: Negative sampling reduces the computational cost by Using a small number Of negative samples For Each positive context pair during training.
    ''' Simpler to implement: It's relatively easier to implement skip-gram with negative sampling compared to hierarchical softmax.
    ''' Performs well With large vocabularies: Negative sampling Is well-suited For training word embeddings With large vocabularies As it scales well.
    ''' Cons:
    ''' May sacrifice quality: With negative sampling, some negative samples may Not be truly informative, potentially leading To a slight degradation In the quality Of learned word embeddings compared To hierarchical softmax.
    ''' Tuning hyperparameters: The effectiveness Of negative sampling depends On the selection Of the number Of negative samples And learning rate, which may require tuning. 
    ''' </summary>
    Public Class WordEmbeddingsWithNegativeSampling
        Inherits WordEmbeddingsModel
        Public NumNegativeSamples As Integer = 5 ' Number of negative samples per positive sample.

        Public Sub New(ByRef Vocabulary As List(Of String), Optional NumberOfNegativeSamples As Integer = 5)
            MyBase.New(Vocabulary)
            Me.NumNegativeSamples = NumberOfNegativeSamples
        End Sub
        Public Sub New(ByRef model As WordEmbeddingsModel)
            MyBase.New(model)
        End Sub
        Public Overrides Sub train()
            ' Initialize word embeddings randomly.
            For Each word In Vocabulary
                WordEmbeddings.Add(word, Enumerable.Range(0, EmbeddingSize).Select(Function(_i) Rand.NextDouble() - 0.5).ToArray())
            Next

            ' Simulate training data (context pairs).
            Dim trainingData As New List(Of (String, String))()
            For i As Integer = 0 To Vocabulary.Count - 1
                For j As Integer = Math.Max(0, i - WindowSize) To Math.Min(Vocabulary.Count - 1, i + WindowSize)
                    If i <> j Then
                        trainingData.Add((Vocabulary(i), Vocabulary(j)))
                    End If
                Next
            Next

            ' Training loop.
            For epoch As Integer = 1 To NumEpochs
                Console.WriteLine($"Training Epoch {epoch}/{NumEpochs}")

                ' Shuffle the training data to avoid learning order biases.
                trainingData = trainingData.OrderBy(Function(_item) Rand.Next()).ToList()

                ' Gradient descent for each context pair.
                For Each item In trainingData
                    ' Generate negative samples.
                    Dim negativeSamples As New List(Of String)()
                    While negativeSamples.Count < NumNegativeSamples
                        Dim randomWord = Vocabulary(Rand.Next(Vocabulary.Count))
                        If randomWord <> item.Item1 AndAlso randomWord <> item.Item2 AndAlso Not negativeSamples.Contains(randomWord) Then
                            negativeSamples.Add(randomWord)
                        End If
                    End While

                    ' Compute the gradients and update the word embeddings.
                    Update(item.Item1, item.Item2, negativeSamples)
                Next
            Next

            ' Print the learned word embeddings.
            For Each word In Vocabulary
                Console.WriteLine($"{word}: {String.Join(", ", WordEmbeddings.GetVector(word))}")
            Next

            ' Now you have learned word embeddings for the given vocabulary.
        End Sub


        Private Sub Update(targetWord As String, contextWord As String, negativeSamples As List(Of String))
            Dim targetEmbedding = WordEmbeddings.GetVector(targetWord)
            Dim contextEmbedding = WordEmbeddings.GetVector(contextWord)

            Dim targetLoss As Double = 0
            Dim contextLoss As Double = 0

            ' Compute the loss for the positive context pair.
            Dim positiveScore As Double = ComputeDotProduct(targetEmbedding, contextEmbedding)
            Dim positiveSigmoid As Double = Sigmoid(positiveScore)
            targetLoss += -Math.Log(positiveSigmoid)
            contextLoss += -Math.Log(positiveSigmoid)

            ' Compute the loss for the negative samples.
            For Each negativeWord In negativeSamples
                Dim negativeEmbedding = WordEmbeddings.GetVector(negativeWord)
                Dim negativeScore As Double = ComputeDotProduct(targetEmbedding, negativeEmbedding)
                Dim negativeSigmoid As Double = Sigmoid(negativeScore)
                targetLoss += -Math.Log(1 - negativeSigmoid)
            Next

            ' Compute the gradients and update the word embeddings.
            Dim targetGradient = contextEmbedding.Clone()
            Dim contextGradient = targetEmbedding.Clone()

            targetGradient = targetGradient.Select(Function(g) g * (positiveSigmoid - 1)).ToArray()
            contextGradient = contextGradient.Select(Function(g) g * (positiveSigmoid - 1)).ToArray()

            For Each negativeWord In negativeSamples
                Dim negativeEmbedding = WordEmbeddings.GetVector(negativeWord)
                Dim negativeSigmoid As Double = Sigmoid(ComputeDotProduct(targetEmbedding, negativeEmbedding))

                For i As Integer = 0 To EmbeddingSize - 1
                    targetGradient(i) += negativeSigmoid * negativeEmbedding(i)
                    negativeEmbedding(i) += negativeSigmoid * targetEmbedding(i)
                Next
            Next

            ' Update the word embeddings using the computed gradients.
            For i As Integer = 0 To EmbeddingSize - 1
                targetEmbedding(i) -= LearningRate * targetGradient(i)
                contextEmbedding(i) -= LearningRate * contextGradient(i)
            Next
        End Sub



    End Class
    ''' <summary>
    '''Hierarchical Softmax
    ''' Pros:
    ''' Theoretically more accurate: Hierarchical softmax provides more accurate training by transforming the softmax operation into a binary tree-based probability calculation, ensuring that Each word Is considered during training.
    ''' Better performance With smaller datasets: Hierarchical softmax Is more suitable For smaller datasets, where negative sampling might Not perform As well due To a limited number Of contexts.
    ''' Cons:
    ''' Computationally expensive For large vocabularies: Hierarchical softmax can become computationally expensive With larger vocabularies, As it requires traversing a binary tree To compute probabilities For Each word during training.
    ''' More complex To implement: Implementing hierarchical softmax can be more complex compared To negative sampling.
    ''' </summary>
    Public Class WordEmbeddingsWithHierarchicalSoftmax
        Inherits WordEmbeddingsModel
        Public Sub New(ByRef model As WordEmbeddingsModel)
            MyBase.New(model)
        End Sub
        Public Sub New(ByRef Vocabulary As List(Of String))
            MyBase.New(Vocabulary)
        End Sub

        Public Overrides Sub Train()
            ' Initialize word embeddings randomly.
            For Each word In Vocabulary
                WordEmbeddings.Add(word, Enumerable.Range(0, EmbeddingSize).Select(Function(_i) Rand.NextDouble() - 0.5).ToArray())
            Next

            ' Simulate training data (context pairs).
            Dim trainingData As New List(Of (String, String))()
            For i As Integer = 0 To Vocabulary.Count - 1
                For j As Integer = Math.Max(0, i - WindowSize) To Math.Min(Vocabulary.Count - 1, i + WindowSize)
                    If i <> j Then
                        trainingData.Add((Vocabulary(i), Vocabulary(j)))
                    End If
                Next
            Next

            ' Training loop.
            For epoch As Integer = 1 To NumEpochs
                Console.WriteLine($"Training Epoch {epoch}/{NumEpochs}")

                ' Shuffle the training data to avoid learning order biases.
                trainingData = trainingData.OrderBy(Function(_item) Rand.Next()).ToList()

                ' Gradient descent for each context pair.
                For Each item In trainingData
                    ' Compute the gradients and update the word embeddings.
                    Update(item.Item1, item.Item2)
                Next
            Next

            ' Print the learned word embeddings.
            For Each word In Vocabulary
                Console.WriteLine($"{word}: {String.Join(", ", WordEmbeddings.GetVector(word))}")
            Next

            ' Now you have learned word embeddings for the given vocabulary.
        End Sub

        Private Sub Update(targetWord As String, contextWord As String)
            Dim targetEmbedding = WordEmbeddings.GetVector(targetWord)
            Dim contextEmbedding = WordEmbeddings.GetVector(contextWord)

            Dim pathToTarget = GetPathToWord(targetWord)
            Dim pathToContext = GetPathToWord(contextWord)

            Dim targetLoss As Double = 0
            Dim contextLoss As Double = 0

            ' Compute the loss for the positive context pair.
            Dim positiveScore As Double = 0
            For Each node In pathToContext
                positiveScore += ComputeDotProduct(targetEmbedding, node.Vector)
            Next
            Dim positiveSigmoid As Double = Sigmoid(positiveScore)
            targetLoss += -Math.Log(positiveSigmoid)
            contextLoss += -Math.Log(positiveSigmoid)

            ' Compute the gradients and update the word embeddings.
            For Each node In pathToContext
                Dim sigmoidGradient As Double = (positiveSigmoid - 1.0) * LearningRate

                For i As Integer = 0 To EmbeddingSize - 1
                    node.Vector(i) -= sigmoidGradient * targetEmbedding(i)
                    targetEmbedding(i) -= sigmoidGradient * node.Vector(i)
                Next
            Next
        End Sub

        Private Function GetPathToWord(word As String) As List(Of Node)
            Dim path As New List(Of Node)()
            Dim currentNode As Node = New Node(WordEmbeddings.GetVector(word))

            While currentNode IsNot Nothing
                path.Add(currentNode)
                currentNode = currentNode.Parent
            End While

            Return path
        End Function



        ' Helper class to represent nodes in the hierarchical softmax binary tree.
        Private Class Node
            Public Property Vector As Double()
            Public Property Left As Node
            Public Property Right As Node
            Public Property Parent As Node

            Public Sub New(vector As Double())
                Me.Vector = vector
            End Sub
        End Class

    End Class


    Public MustInherit Class WordEmbeddingsModel

        ' A simple vocabulary for demonstration purposes.
        Private iVocabulary As New List(Of String) From {"apple", "orange", "banana", "grape", "cherry"}
        Public ReadOnly Property Vocabulary As List(Of String)
            Get
                Return iVocabulary
            End Get
        End Property
        ' Word embeddings dictionary to store the learned word vectors.
        Public WordEmbeddings As New WordEmbedding

        ' Hyperparameters for training.
        Public EmbeddingSize As Integer = 50 ' Size of word vectors.
        Public WindowSize As Integer = 2 ' Context window size.

        Public LearningRate As Double = 0.01 ' Learning rate for gradient descent.
        Public NumEpochs As Integer = 1000 ' Number of training epochs.

        ' Random number generator for initialization.
        Public Shared ReadOnly Rand As New Random()
        Public MustOverride Sub Train()
        Public Sub New(ByRef model As WordEmbeddingsModel)
            iVocabulary = model.Vocabulary
            WordEmbeddings = model.WordEmbeddings
            EmbeddingSize = model.EmbeddingSize
            WindowSize = model.WindowSize
            LearningRate = model.LearningRate
            NumEpochs = model.NumEpochs
        End Sub
        Public Sub New(ByRef Vocabulary As List(Of String))
            iVocabulary = Vocabulary
        End Sub
        Public Function ExportModel() As WordEmbeddingsModel
            Return Me
        End Function
        Public Sub SetTrainingParameters(ByRef Embeddingsize As Integer,
                                         ByRef WindowSize As Integer,
                                         ByRef LearningRate As Double, ByRef Epochs As Integer)
            Me.EmbeddingSize = Embeddingsize
            Me.WindowSize = WindowSize
            Me.LearningRate = LearningRate
            Me.NumEpochs = Epochs
        End Sub
        ' WordEmbedding class to store word vectors and handle operations on word embeddings.
        Public Class WordEmbedding
            Private ReadOnly embeddings As Dictionary(Of String, Double())

            Public Sub New()
                Me.embeddings = New Dictionary(Of String, Double())()
            End Sub

            Public Sub Add(word As String, vector As Double())
                embeddings(word) = vector
            End Sub

            Public Function GetVector(word As String) As Double()
                Return embeddings(word)
            End Function

            ' Implement other operations as needed for word embeddings.
            ' E.g., similarity, word lookup, etc.
        End Class
        Public Function ComputeDotProduct(vec1 As Double(), vec2 As Double()) As Double
            Return Enumerable.Range(0, EmbeddingSize).Sum(Function(i) vec1(i) * vec2(i))
        End Function

        Public Function Sigmoid(x As Double) As Double
            Return 1.0 / (1.0 + Math.Exp(-x))
        End Function

        ''' <summary>
        ''' Cosine Similarity(A, B) = (A dot B) / (||A|| * ||B||)
        '''  where:
        '''  A And B are the word vectors of two words.
        '''  A dot B Is the dot product Of the two vectors.
        '''  ||A|| And ||B|| are the magnitudes (Euclidean norms) of the vectors.
        '''  The cosine similarity ranges from -1 To 1, where 1 indicates the highest similarity, 0 indicates no similarity, And -1 indicates the highest dissimilarity.
        ''' </summary>
        ''' <param name="word1"></param>
        ''' <param name="word2"></param>
        ''' <param name="wordEmbeddings"></param>
        ''' <returns></returns>
        Public Function CosineSimilarity(word1 As String, word2 As String, wordEmbeddings As WordEmbedding) As Double
            Dim vector1 As Double() = wordEmbeddings.GetVector(word1)
            Dim vector2 As Double() = wordEmbeddings.GetVector(word2)

            ' Calculate the dot product of the two vectors.
            Dim dotProduct As Double = 0
            For i As Integer = 0 To vector1.Length - 1
                dotProduct += vector1(i) * vector2(i)
            Next

            ' Calculate the magnitudes of the vectors.
            Dim magnitude1 As Double = Math.Sqrt(vector1.Sum(Function(x) x * x))
            Dim magnitude2 As Double = Math.Sqrt(vector2.Sum(Function(x) x * x))

            ' Calculate the cosine similarity.
            Dim similarity As Double = dotProduct / (magnitude1 * magnitude2)

            Return similarity
        End Function
    End Class

    ''' <summary>
    ''' One possible way to combine the approaches is by using a two-step training process:
    '''  Pre-training Using Skip-gram With Negative Sampling:
    '''   In this step, 
    '''    you can pre-train the word embeddings using the skip-gram model 
    '''    with negative sampling on a large dataset Or a diverse corpus. 
    '''    This step allows you to efficiently learn word embeddings 
    '''    in a computationally efficient 
    '''    manner while capturing semantic relationships between words.
    '''  Fine-tuning using Hierarchical Softmax:
    '''   After pre-training the word embeddings, 
    '''    you can perform fine-tuning Using the hierarchical softmax technique. 
    '''    During the fine-tuning Step, 
    '''    you can use a smaller dataset 
    '''   Or a more domain-specific corpus 
    '''    To train the model Using hierarchical softmax. 
    '''    This Step enables you To refine the word embeddings 
    '''    And make them more accurate And context-specific.
    ''' </summary>
    Public Class HybridWordEmbeddingsModel
        Inherits WordEmbeddingsModel

        Public Sub New(ByRef model As WordEmbeddingsModel)
            MyBase.New(model)
        End Sub

        Public Sub New(ByRef Vocabulary As List(Of String))
            MyBase.New(Vocabulary)
        End Sub
        Public Enum ModelType
            Skipgram
            Glove
            SoftMax
            CBOW
            FastText
        End Enum
        Public Function PreTrain(ByRef model As WordEmbeddingsModel, ByRef iModelType As ModelType) As WordEmbeddingsModel
            model.Train()
            Dim preTrainedModel As WordEmbeddingsModel


            Select Case iModelType
                Case ModelType.Skipgram
                    preTrainedModel = New WordEmbeddingsWithNegativeSampling(model.Vocabulary)
                    preTrainedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs)  ' Set appropriate parameters for pre-training

                    preTrainedModel.Train() ' Pre-train the word embeddings using Skip-gram with Negative Sampling

                    Return preTrainedModel
                Case ModelType.Glove
                    preTrainedModel = New WordEmbeddingsWithGloVe(model.Vocabulary)
                    preTrainedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs)  ' Set appropriate parameters for pre-training

                    preTrainedModel.Train() ' Pre-train the word embeddings using Skip-gram with Negative Sampling

                    Return preTrainedModel
                Case ModelType.FastText
                    preTrainedModel = New WordEmbeddingsWithFastText(model.Vocabulary)
                    preTrainedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs)  ' Set appropriate parameters for pre-training

                    preTrainedModel.Train() ' Pre-train the word embeddings using Skip-gram with Negative Sampling

                    Return preTrainedModel
                Case ModelType.CBOW
                    preTrainedModel = New WordEmbeddingsWithCBOW(model.Vocabulary)
                    preTrainedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs)  ' Set appropriate parameters for pre-training

                    preTrainedModel.Train() ' Pre-train the word embeddings using Skip-gram with Negative Sampling

                    Return preTrainedModel
                Case ModelType.SoftMax
                    preTrainedModel = New WordEmbeddingsWithHierarchicalSoftmax(model.Vocabulary)
                    preTrainedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs)  ' Set appropriate parameters for pre-training

                    preTrainedModel.Train() ' Pre-train the word embeddings using Skip-gram with Negative Sampling

                    Return preTrainedModel
            End Select
            Return model
        End Function

        Public Function FineTune(ByRef Model As WordEmbeddingsModel, ByRef iModelType As ModelType) As WordEmbeddingsModel

            Dim fineTunedModel As WordEmbeddingsModel

            Model.Train()

            Select Case iModelType
                Case ModelType.CBOW
                    fineTunedModel = New WordEmbeddingsWithCBOW(Model.Vocabulary)
                    fineTunedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs) ' Set appropriate parameters for fine-tuning
                    fineTunedModel.Train() ' Fine-tune the word embeddings using Hierarchical Softmax
                    Return fineTunedModel
                Case ModelType.FastText
                    fineTunedModel = New WordEmbeddingsWithFastText(Model.Vocabulary)
                    fineTunedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs) ' Set appropriate parameters for fine-tuning
                    fineTunedModel.Train() ' Fine-tune the word embeddings using Hierarchical Softmax
                    Return fineTunedModel
                Case ModelType.Glove
                    fineTunedModel = New WordEmbeddingsWithGloVe(Model.Vocabulary)
                    fineTunedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs) ' Set appropriate parameters for fine-tuning
                    fineTunedModel.Train() ' Fine-tune the word embeddings using Hierarchical Softmax
                    Return fineTunedModel
                Case ModelType.Skipgram
                    fineTunedModel = New WordEmbeddingsWithNegativeSampling(Model.Vocabulary)
                    fineTunedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs) ' Set appropriate parameters for fine-tuning
                    fineTunedModel.Train() ' Fine-tune the word embeddings using Hierarchical Softmax
                    Return fineTunedModel
                Case ModelType.SoftMax
                    fineTunedModel = New WordEmbeddingsWithHierarchicalSoftmax(Model.Vocabulary)
                    fineTunedModel.SetTrainingParameters(EmbeddingSize, WindowSize, LearningRate, NumEpochs) ' Set appropriate parameters for fine-tuning
                    fineTunedModel.Train() ' Fine-tune the word embeddings using Hierarchical Softmax
                    Return fineTunedModel

            End Select


            Return Model

        End Function

        Public Overrides Sub Train()
            Dim hybrid As New HybridWordEmbeddingsModel(Vocabulary)
            Dim preTrainedModel = PreTrain(hybrid, ModelType.Skipgram)
            Dim fineTunedModel = FineTune(preTrainedModel, ModelType.SoftMax)
            'set model
            Me.WordEmbeddings = fineTunedModel.WordEmbeddings

        End Sub
    End Class


    Public Class WordEmbeddingsWithGloVe
        Inherits WordEmbeddingsModel

        Public Sub New(ByRef model As WordEmbeddingsModel)
            MyBase.New(model)
        End Sub

        Public Sub New(ByRef Vocabulary As List(Of String))
            MyBase.New(Vocabulary)
        End Sub

        Public Overrides Sub Train()
            ' Initialize word embeddings randomly.
            For Each word In Vocabulary
                WordEmbeddings.Add(word, Enumerable.Range(0, EmbeddingSize).Select(Function(_i) Rand.NextDouble() - 0.5).ToArray())
            Next

            ' Construct the global word co-occurrence matrix.
            Dim coOccurrenceMatrix = BuildCoOccurrenceMatrix()

            ' Training loop.
            For epoch As Integer = 1 To NumEpochs
                Console.WriteLine($"Training Epoch {epoch}/{NumEpochs}")

                ' Shuffle the training data to avoid learning order biases.
                coOccurrenceMatrix = coOccurrenceMatrix.OrderBy(Function(_item) Rand.Next()).ToList()

                ' Gradient descent for each word pair in the co-occurrence matrix.
                For Each item In coOccurrenceMatrix
                    ' Compute the gradients and update the word embeddings.
                    Update(item.Item1, item.Item2, item.Item3)
                Next
            Next

            ' Print the learned word embeddings.
            For Each word In Vocabulary
                Console.WriteLine($"{word}: {String.Join(", ", WordEmbeddings.GetVector(word))}")
            Next

            ' Now you have learned word embeddings for the given vocabulary.
        End Sub

        Private Function BuildCoOccurrenceMatrix() As List(Of (String, String, Double))
            ' Construct a global word co-occurrence matrix.
            Dim coOccurrenceMatrix As New List(Of (String, String, Double))()

            ' Simulate training data (context pairs).
            For i As Integer = 0 To Vocabulary.Count - 1
                For j As Integer = Math.Max(0, i - WindowSize) To Math.Min(Vocabulary.Count - 1, i + WindowSize)
                    If i <> j Then
                        ' Increment the co-occurrence count for the word pair (Vocabulary(i), Vocabulary(j)).
                        Dim coOccurrenceCount = 1.0 / (Math.Abs(i - j))
                        coOccurrenceMatrix.Add((Vocabulary(i), Vocabulary(j), coOccurrenceCount))
                    End If
                Next
            Next

            Return coOccurrenceMatrix
        End Function

        Private Sub Update(word1 As String, word2 As String, coOccurrenceCount As Double)
            Dim vector1 = WordEmbeddings.GetVector(word1)
            Dim vector2 = WordEmbeddings.GetVector(word2)

            Dim dotProduct As Double = ComputeDotProduct(vector1, vector2)
            Dim loss As Double = (dotProduct - Math.Log(coOccurrenceCount)) ^ 2

            Dim gradient1 = New Double(EmbeddingSize - 1) {}
            Dim gradient2 = New Double(EmbeddingSize - 1) {}

            For i As Integer = 0 To EmbeddingSize - 1
                gradient1(i) = 2.0 * (dotProduct - Math.Log(coOccurrenceCount)) * vector2(i)
                gradient2(i) = 2.0 * (dotProduct - Math.Log(coOccurrenceCount)) * vector1(i)
            Next

            ' Update the word embeddings using the computed gradients.
            For i As Integer = 0 To EmbeddingSize - 1
                vector1(i) -= LearningRate * gradient1(i)
                vector2(i) -= LearningRate * gradient2(i)
            Next
        End Sub

    End Class

    Public Class WordEmbeddingsWithFastText
        Inherits WordEmbeddingsModel

        Public Sub New(ByRef model As WordEmbeddingsModel)
            MyBase.New(model)
        End Sub

        Public Sub New(ByRef vocabulary As List(Of String))
            MyBase.New(vocabulary)
        End Sub

        Public Overrides Sub Train()
            ' Initialize word embeddings randomly.
            For Each word In Vocabulary
                WordEmbeddings.Add(word, Enumerable.Range(0, EmbeddingSize).Select(Function(_i) Rand.NextDouble() - 0.5).ToArray())
            Next

            ' Simulate training data (context pairs).
            Dim trainingData As New List(Of (String, String))()
            For i As Integer = 0 To Vocabulary.Count - 1
                For j As Integer = Math.Max(0, i - WindowSize) To Math.Min(Vocabulary.Count - 1, i + WindowSize)
                    If i <> j Then
                        trainingData.Add((Vocabulary(i), Vocabulary(j)))
                    End If
                Next
            Next

            ' Training loop.
            For epoch As Integer = 1 To NumEpochs
                Console.WriteLine($"Training Epoch {epoch}/{NumEpochs}")

                ' Shuffle the training data to avoid learning order biases.
                trainingData = trainingData.OrderBy(Function(_item) Rand.Next()).ToList()

                ' Gradient descent for each context pair.
                For Each item In trainingData
                    ' Compute the gradients and update the word embeddings.
                    Update(item.Item1, item.Item2)
                Next
            Next

            ' Print the learned word embeddings.
            For Each word In Vocabulary
                Console.WriteLine($"{word}: {String.Join(", ", WordEmbeddings.GetVector(word))}")
            Next

            ' Now you have learned word embeddings for the given vocabulary.
        End Sub

        Private Sub Update(targetWord As String, contextWord As String)
            Dim targetEmbedding = WordEmbeddings.GetVector(targetWord)
            Dim contextEmbedding = WordEmbeddings.GetVector(contextWord)

            Dim targetLoss As Double = 0
            Dim contextLoss As Double = 0

            ' Compute the loss for the positive context pair.
            Dim positiveScore As Double = ComputeDotProduct(targetEmbedding, contextEmbedding)
            Dim positiveSigmoid As Double = Sigmoid(positiveScore)
            targetLoss += -Math.Log(positiveSigmoid)
            contextLoss += -Math.Log(positiveSigmoid)

            ' Compute the gradients and update the word embeddings.
            Dim targetGradient = contextEmbedding.Clone()
            Dim contextGradient = targetEmbedding.Clone()

            targetGradient = targetGradient.Select(Function(g) g * (positiveSigmoid - 1)).ToArray()
            contextGradient = contextGradient.Select(Function(g) g * (positiveSigmoid - 1)).ToArray()

            ' Update the word embeddings using the computed gradients.
            For i As Integer = 0 To EmbeddingSize - 1
                targetEmbedding(i) -= LearningRate * targetGradient(i)
                contextEmbedding(i) -= LearningRate * contextGradient(i)
            Next
        End Sub
    End Class
    Public Class WordEmbeddingsWithCBOW
        Inherits WordEmbeddingsModel

        Public Sub New(ByRef model As WordEmbeddingsModel)
            MyBase.New(model)
        End Sub

        Public Sub New(ByRef Vocabulary As List(Of String))
            MyBase.New(Vocabulary)
        End Sub

        Public Overrides Sub Train()
            ' Initialize word embeddings randomly.
            For Each word In Vocabulary
                WordEmbeddings.Add(word, Enumerable.Range(0, EmbeddingSize).Select(Function(_i) Rand.NextDouble() - 0.5).ToArray())
            Next

            ' Simulate training data (context pairs).
            Dim trainingData As New List(Of (List(Of String), String))()
            For i As Integer = 0 To Vocabulary.Count - 1
                Dim contextWords As New List(Of String)()
                For j As Integer = Math.Max(0, i - WindowSize) To Math.Min(Vocabulary.Count - 1, i + WindowSize)
                    If i <> j Then
                        contextWords.Add(Vocabulary(j))
                    End If
                Next
                If contextWords.Count > 0 Then
                    trainingData.Add((contextWords, Vocabulary(i)))
                End If
            Next

            ' Training loop.
            For epoch As Integer = 1 To NumEpochs
                Console.WriteLine($"Training Epoch {epoch}/{NumEpochs}")

                ' Shuffle the training data to avoid learning order biases.
                trainingData = trainingData.OrderBy(Function(_item) Rand.Next()).ToList()

                ' Gradient descent for each context pair.
                For Each item In trainingData
                    ' Compute the gradients and update the word embeddings.
                    Update(item.Item1, item.Item2)
                Next
            Next

            ' Print the learned word embeddings.
            For Each word In Vocabulary
                Console.WriteLine($"{word}: {String.Join(", ", WordEmbeddings.GetVector(word))}")
            Next

            ' Now you have learned word embeddings for the given vocabulary.
        End Sub

        Private Sub Update(contextWords As List(Of String), targetWord As String)
            Dim contextEmbeddings = contextWords.Select(Function(word) WordEmbeddings.GetVector(word)).ToList()
            Dim targetEmbedding = WordEmbeddings.GetVector(targetWord)

            ' Average the context embeddings.
            Dim averageContext = New Double(EmbeddingSize - 1) {}
            For Each context In contextEmbeddings
                For i As Integer = 0 To EmbeddingSize - 1
                    averageContext(i) += context(i)
                Next
            Next

            For i As Integer = 0 To EmbeddingSize - 1
                averageContext(i) /= contextEmbeddings.Count
            Next

            ' Compute the loss for the target word.
            Dim targetLoss As Double = 0
            Dim positiveScore As Double = ComputeDotProduct(targetEmbedding, averageContext)
            Dim positiveSigmoid As Double = Sigmoid(positiveScore)
            targetLoss += -Math.Log(positiveSigmoid)

            ' Compute the gradient and update the word embeddings.
            Dim targetGradient = averageContext.Select(Function(g) g * (positiveSigmoid - 1)).ToArray()

            For Each context In contextEmbeddings
                Dim sigmoidGradient As Double = (positiveSigmoid - 1.0) * LearningRate

                For i As Integer = 0 To EmbeddingSize - 1
                    context(i) -= sigmoidGradient * targetEmbedding(i)
                    targetGradient(i) += sigmoidGradient * context(i)
                Next
            Next

            ' Update the word embeddings using the computed gradients.
            For i As Integer = 0 To EmbeddingSize - 1
                targetEmbedding(i) -= LearningRate * targetGradient(i)
            Next
        End Sub
    End Class
End Namespace




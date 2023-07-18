
''' <summary>
''' Returns a list WordGram Probability Given a Sequence of Tokens 
''' </summary>
Public Class WordgramModel
    Private n As Integer
    Public Shared Sub Main()
        ' Train the wordgram model
        Dim trainingData As New List(Of String) From {"I love cats and dogs.", "Dogs are loyal companions."}
        Dim words As New List(Of String) From {
            "apple", "banana", "orange", "apple", "pear", "kiwi", "orange", "mango", "kiwi", "guava", "kiwi", "orange", "orange", "apple", "banana"
        }
        Dim sentences As New List(Of String) From {
            "I love apples.",
            "Bananas are tasty.",
            "I love apples.",
            "I enjoy eating bananas.",
            "mango is a delicious fruit.", "Bananas are tasty.",
            "I love apples.", "I enjoy eating bananas.",
            "Kiwi is a delicious fruit.", "I love apples.",
            "I enjoy eating bananas.",
            "orange is a delicious fruit.", "I love apples.",
            "I enjoy eating bananas.",
            "Kiwi is a delicious fruit.", "Bananas are tasty."
        }
        Dim Corpus As New List(Of String)
        Corpus.AddRange(sentences)
        Corpus.AddRange(words)


        ' Generate a sentence using the wordgram model
        For I = 1 To 5
            Dim wordgramModel As New WordgramModel(Corpus, I)
            Dim generatedSentence As String = wordgramModel.GenerateSentence()
            Console.WriteLine(generatedSentence)
        Next I
        Console.ReadLine()
    End Sub

    Public wordgramCounts As New Dictionary(Of List(Of String), Integer)
    Public wordgramProbabilities As New Dictionary(Of List(Of String), Double)
    Public Sub New(trainingData As List(Of String), n As Integer)
        Me.n = n
        TrainModel(trainingData)
    End Sub
    Private Sub TrainModel(trainingData As List(Of String))
        ' Preprocess training data and tokenize into wordgrams
        Dim wordgrams As New List(Of List(Of String))
        For Each sentence As String In trainingData
            Dim tokens() As String = sentence.Split(" "c)
            For i As Integer = 0 To tokens.Length - n
                Dim wordgram As List(Of String) = tokens.Skip(i).Take(n).ToList()
                wordgrams.Add(wordgram)
            Next
        Next

        ' Count wordgrams
        For Each wordgram As List(Of String) In wordgrams
            If wordgramCounts.ContainsKey(wordgram) Then
                wordgramCounts(wordgram) += 1
            Else
                wordgramCounts.Add(wordgram, 1)
            End If
        Next

        ' Calculate wordgram probabilities
        Dim totalCount As Integer = wordgramCounts.Values.Sum()
        For Each wordgram As List(Of String) In wordgramCounts.Keys
            Dim count As Integer = wordgramCounts(wordgram)
            Dim probability As Double = count / totalCount
            wordgramProbabilities.Add(wordgram, probability)
        Next
    End Sub
    Private Function GenerateNextWord(wordgram As List(Of String)) As String
        Dim random As New Random()
        Dim candidates As New List(Of String)
        Dim probabilities As New List(Of Double)

        ' Collect candidate words and their probabilities
        For Each candidateWordgram As List(Of String) In wordgramCounts.Keys
            If candidateWordgram.GetRange(0, n - 1).SequenceEqual(wordgram) Then
                Dim candidateWord As String = candidateWordgram.Last()
                Dim probability As Double = wordgramProbabilities(candidateWordgram)
                candidates.Add(candidateWord)
                probabilities.Add(probability)
            End If
        Next

        ' Randomly select a candidate word based on probabilities
        Dim totalProbability As Double = probabilities.Sum()
        Dim randomValue As Double = random.NextDouble() * totalProbability
        Dim cumulativeProbability As Double = 0

        For i As Integer = 0 To candidates.Count - 1
            cumulativeProbability += probabilities(i)
            If randomValue <= cumulativeProbability Then
                Return candidates(i)
            End If
        Next

        Return ""
    End Function
    Public Function GenerateSentence() As String
        Dim sentence As New List(Of String)
        Dim random As New Random()

        ' Start the sentence with a random wordgram
        Dim randomIndex As Integer = random.Next(0, wordgramCounts.Count)
        Dim currentWordgram As List(Of String) = wordgramCounts.Keys(randomIndex)
        sentence.AddRange(currentWordgram)

        ' Generate subsequent words based on wordgram probabilities
        While wordgramCounts.ContainsKey(currentWordgram)
            Dim nextWord As String = GenerateNextWord(currentWordgram)
            If nextWord = "" Then
                Exit While
            End If
            sentence.Add(nextWord)

            ' Backoff to lower-order wordgrams if necessary
            If currentWordgram.Count > 1 Then
                currentWordgram.RemoveAt(0)
            Else
                Exit While
            End If
            currentWordgram.Add(nextWord)
        End While

        Return String.Join(" ", sentence)
    End Function
    Private Sub Train(trainingData As List(Of String))
        ' Preprocess training data and tokenize into wordgrams
        Dim wordgrams As New List(Of List(Of String))
        For Each sentence As String In trainingData
            Dim tokens() As String = sentence.Split(" "c)
            For i As Integer = 0 To tokens.Length - n
                Dim wordgram As List(Of String) = tokens.Skip(i).Take(n).ToList()
                wordgrams.Add(wordgram)
            Next
        Next

        ' Count wordgrams
        For Each wordgram As List(Of String) In wordgrams
            If wordgramCounts.ContainsKey(wordgram) Then
                wordgramCounts(wordgram) += 1
            Else
                wordgramCounts.Add(wordgram, 1)
            End If
        Next

        ' Calculate wordgram probabilities based on frequency-based distribution
        For Each wordgram As List(Of String) In wordgramCounts.Keys
            Dim count As Integer = wordgramCounts(wordgram)
            Dim order As Integer = wordgram.Count

            ' Calculate the frequency threshold for higher-order n-grams
            Dim frequencyThreshold As Integer = 5 ' Set your desired threshold
            If order = n AndAlso count >= frequencyThreshold Then
                wordgramProbabilities.Add(wordgram, count)
            ElseIf order < n AndAlso count >= frequencyThreshold Then
                ' Assign the frequency to lower-order n-grams
                Dim lowerOrderWordgram As List(Of String) = wordgram.Skip(1).ToList()
                If wordgramProbabilities.ContainsKey(lowerOrderWordgram) Then
                    wordgramProbabilities(lowerOrderWordgram) += count
                Else
                    wordgramProbabilities.Add(lowerOrderWordgram, count)
                End If
            End If
        Next

        ' Normalize probabilities within each order
        For order As Integer = 1 To n
            Dim totalProbability As Double = 0
            For Each wordgram As List(Of String) In wordgramProbabilities.Keys.ToList()
                If wordgram.Count = order Then
                    totalProbability += wordgramProbabilities(wordgram)
                End If
            Next
            For Each wordgram As List(Of String) In wordgramProbabilities.Keys.ToList()
                If wordgram.Count = order Then
                    wordgramProbabilities(wordgram) /= totalProbability
                End If
            Next
        Next
    End Sub


End Class

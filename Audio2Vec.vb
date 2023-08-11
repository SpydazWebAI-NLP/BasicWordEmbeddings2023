Imports System.Numerics
Imports MathNet.Numerics.IntegralTransforms

Public Class Audio2Vec
    Public Shared Function AudioToVector(audioSignal As Double(), windowSize As Integer, hopSize As Integer) As List(Of Complex())
        Dim vectors As New List(Of Complex())

        For i As Integer = 0 To audioSignal.Length - windowSize Step hopSize
            Dim window(windowSize - 1) As Complex

            For j As Integer = 0 To windowSize - 1
                window(j) = audioSignal(i + j)
            Next

            Dim spectrum As Complex() = CalculateSpectrum(window)
            vectors.Add(spectrum)
        Next

        Return vectors
    End Function
    Public Shared Function VectorToAudio(vectors As List(Of Complex()), hopSize As Integer) As Double()
        Dim audioSignal As New List(Of Double)()

        For Each spectrum As Complex() In vectors
            Dim windowSignal As Double() = CalculateInverseSpectrum(spectrum)

            For i As Integer = 0 To hopSize - 1
                If audioSignal.Count + i < windowSignal.Length Then
                    audioSignal.Add(windowSignal(i))
                End If
            Next
        Next

        Return audioSignal.ToArray()
    End Function
    Public Shared Function VectorToAudio(vectors As List(Of Complex()), windowSize As Integer, hopSize As Integer) As Double()
        Dim audioSignal As New List(Of Double)()

        ' Initialize a buffer to accumulate audio segments
        Dim buffer(audioSignal.Count + windowSize - 1) As Double

        For Each spectrum As Complex() In vectors
            Dim windowSignal As Double() = CalculateInverseSpectrum(spectrum)

            For i As Integer = 0 To hopSize - 1
                If audioSignal.Count + i < windowSignal.Length Then
                    ' Add the current window to the buffer
                    buffer(audioSignal.Count + i) += windowSignal(i)
                End If
            Next

            ' Check if there's enough data in the buffer to generate a full segment
            While buffer.Length >= hopSize
                ' Extract a segment from the buffer and add to the audio signal
                Dim segment(hopSize - 1) As Double
                Array.Copy(buffer, segment, hopSize)
                audioSignal.AddRange(segment)

                ' Shift the buffer by hopSize
                Dim newBuffer(buffer.Length - hopSize - 1) As Double
                Array.Copy(buffer, hopSize, newBuffer, 0, newBuffer.Length)
                buffer = newBuffer
            End While
        Next

        ' Convert the remaining buffer to audio
        audioSignal.AddRange(buffer)

        Return audioSignal.ToArray()
    End Function
    Public Shared Function LoadAudio(audioPath As String) As Double()
        Dim audioData As New List(Of Double)()

        ' Use NAudio or other library to load audio data from the specified audioPath
        Using reader As New NAudio.Wave.AudioFileReader(audioPath)
            Dim buffer(reader.WaveFormat.SampleRate * reader.WaveFormat.Channels - 1) As Single
            While reader.Read(buffer, 0, buffer.Length) > 0
                For Each sample As Single In buffer
                    audioData.Add(CDbl(sample))
                Next
            End While
        End Using

        Return audioData.ToArray()
    End Function
    Public Shared Sub SaveAudio(audioSignal As Double(), outputPath As String)
        ' Convert the array of doubles to an array of singles for NAudio
        Dim audioData(audioSignal.Length - 1) As Single
        For i As Integer = 0 To audioSignal.Length - 1
            audioData(i) = CSng(audioSignal(i))
        Next

        ' Use NAudio or other library to save audio data to the specified outputPath
        Using writer As New NAudio.Wave.WaveFileWriter(outputPath, New NAudio.Wave.WaveFormat())
            writer.WriteSamples(audioData, 0, audioData.Length)
        End Using
    End Sub
    Private Shared Function CalculateInverseSpectrum(spectrum As Complex()) As Double()
        ' Perform inverse FFT using MathNet.Numerics library
        Fourier.Inverse(spectrum, FourierOptions.Default)

        ' Return the real part of the inverse spectrum as the reconstructed signal
        Dim reconstructedSignal(spectrum.Length - 1) As Double
        For i As Integer = 0 To spectrum.Length - 1
            reconstructedSignal(i) = spectrum(i).Real
        Next
        Return reconstructedSignal
    End Function
    Private Shared Function CalculateSpectrum(window As Complex()) As Complex()
        ' Perform FFT using MathNet.Numerics library
        Fourier.Forward(window, FourierOptions.Default)

        Return window
    End Function
End Class

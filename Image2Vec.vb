Imports System.Drawing
Imports System.Drawing.Imaging

Public Class Image2Vec
    Public Shared Sub SaveVectorToFile(imgVector As Double(), outputPath As String)
        Using writer As New System.IO.StreamWriter(outputPath)
            For Each value As Double In imgVector
                writer.WriteLine(value)
            Next
        End Using
    End Sub

    Public Class ImageDecoder
        Public Sub DecodeImage(imgVector As Double(), width As Integer, height As Integer, outputPath As String)
            Dim decodedImage As New Bitmap(width, height)

            Dim index As Integer = 0
            For y As Integer = 0 To height - 1
                For x As Integer = 0 To width - 1
                    Dim grayscaleValue As Integer = CInt(Math.Floor(imgVector(index) * 255))
                    Dim pixelColor As Color = Color.FromArgb(grayscaleValue, grayscaleValue, grayscaleValue)
                    decodedImage.SetPixel(x, y, pixelColor)
                    index += 1
                Next
            Next

            decodedImage.Save(outputPath, Imaging.ImageFormat.Jpeg)
        End Sub
    End Class

    Public Class ImageEncoder
        Public Function EncodeImage(imagePath As String, width As Integer, height As Integer) As Double()
            Dim resizedImage As Bitmap = ResizeImage(imagePath, width, height)
            Dim grayscaleImage As Bitmap = ConvertToGrayscale(resizedImage)
            Dim pixelValues As Double() = GetPixelValues(grayscaleImage)
            Return pixelValues
        End Function

        Private Function ConvertToGrayscale(image As Bitmap) As Bitmap
            Dim grayscaleImage As New Bitmap(image.Width, image.Height, PixelFormat.Format8bppIndexed)
            Using g As Graphics = Graphics.FromImage(grayscaleImage)
                Dim colorMatrix As ColorMatrix = New ColorMatrix(New Single()() {
                New Single() {0.299F, 0.299F, 0.299F, 0, 0},
                New Single() {0.587F, 0.587F, 0.587F, 0, 0},
                New Single() {0.114F, 0.114F, 0.114F, 0, 0},
                New Single() {0, 0, 0, 1, 0},
                New Single() {0, 0, 0, 0, 1}
            })
                Dim attributes As ImageAttributes = New ImageAttributes()
                attributes.SetColorMatrix(colorMatrix)
                g.DrawImage(image, New Rectangle(0, 0, image.Width, image.Height), 0, 0, image.Width, image.Height, GraphicsUnit.Pixel, attributes)
            End Using
            Return grayscaleImage
        End Function

        Private Function GetPixelValues(image As Bitmap) As Double()
            Dim pixelValues As New List(Of Double)()
            For y As Integer = 0 To image.Height - 1
                For x As Integer = 0 To image.Width - 1
                    Dim pixelColor As Color = image.GetPixel(x, y)
                    Dim grayscaleValue As Double = pixelColor.R / 255.0
                    pixelValues.Add(grayscaleValue)
                Next
            Next
            Return pixelValues.ToArray()
        End Function

        Private Function ResizeImage(imagePath As String, width As Integer, height As Integer) As Bitmap
            Dim originalImage As Bitmap = New Bitmap(imagePath)
            Dim resizedImage As Bitmap = New Bitmap(width, height)
            Using g As Graphics = Graphics.FromImage(resizedImage)
                g.DrawImage(originalImage, 0, 0, width, height)
            End Using
            Return resizedImage
        End Function
    End Class

    Public Class ImageSearch
        Public Function FindSimilarImages(queryVector As Double(), imageVectors As List(Of Tuple(Of String, Double())), numResults As Integer) As List(Of String)
            Dim similarImages As New List(Of String)()

            For Each imageVectorPair As Tuple(Of String, Double()) In imageVectors
                Dim imageName As String = imageVectorPair.Item1
                Dim imageVector As Double() = imageVectorPair.Item2

                Dim distance As Double = CalculateEuclideanDistance(queryVector, imageVector)
                similarImages.Add(imageName)
            Next

            similarImages.Sort() ' Sort the list of similar image names

            Return similarImages.Take(numResults).ToList()
        End Function



        Private Function CalculateEuclideanDistance(vector1 As Double(), vector2 As Double()) As Double
            Dim sumSquaredDifferences As Double = 0
            For i As Integer = 0 To vector1.Length - 1
                Dim difference As Double = vector1(i) - vector2(i)
                sumSquaredDifferences += difference * difference
            Next
            Return Math.Sqrt(sumSquaredDifferences)
        End Function
    End Class
End Class

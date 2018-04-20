package main

import "github.com/go-vgo/robotgo"

func main() {
	width, height := robotgo.GetScreenSize()
	bitmap := robotgo.CaptureScreen(0, 0, width, height)

	robotgo.SaveBitmap(bitmap, "test.png")
	
	bitmap = robotgo.CaptureScreen()//0, 0, width, height)

	robotgo.SaveBitmap(bitmap, "test2.png")
} 
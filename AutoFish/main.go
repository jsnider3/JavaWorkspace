package main

import (
	"fmt"
	//"github.com/micmonay/keybd_event"
	"github.com/go-vgo/robotgo"
	"time"
)

// We want Grindea to be running and programmatically accessible.
func isGrindeaReady() (ready bool) {
	// Check that Grindea is running.
	robotgo.ScrollMouse(50, "up")
	robotgo.MouseClick("left", true)
	// Move Grindea to foreground.
	return true
}

func pressZ() () {
	time.Sleep(100 * time.Millisecond)
	fmt.Printf("Press Z.\n")
	robotgo.TypeString("z")
	/*kb, err := keybd_event.NewKeyBonding()
	kb.SetKeys(keybd_event.VK_Z)
	err = kb.Launching() 
	if err != nil {
		panic(err)
	}*/
}

// Play the minigame as per http://secretsofgrindea.wikia.com/wiki/Fishing.
func playMinigame() () {
	for true {
	// 1. Simulate a 'Z' keypress.
	pressZ()
	// 2. Wait until we have a fish on the hook.
	// 3. Press 'Z' to start reeling in the fish.
	pressZ()
	// 4. Press the left and right keys to play the minigame.
	}
}

func saveScreen() () {
	w, h := robotgo.GetScreenSize()
	bitmap := robotgo.CaptureScreen(0, 0, w, h)
	fmt.Println("...", bitmap)

	robotgo.SaveBitmap(bitmap, "test.png")
}

// main starts the fishing process.
func main() {
	// 1. Ensure that Secrets of Grindea is running in the foreground.
	if isGrindeaReady() {
		// 2. Play the minigame.
		playMinigame()
		// 3. Repeat step 2 until cancelled.
	}
	saveScreen()
}

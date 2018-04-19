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
	// Move Grindea to foreground.
	time.Sleep(10000 * time.Millisecond) // Debug
	return true
}

func pressZ() () {
	fmt.Printf("Press Z.\n")
	robotgo.KeyTap("z")
	/*kb, err := keybd_event.NewKeyBonding()
	kb.SetKeys(keybd_event.VK_Z)
	err = kb.Launching() 
	if err != nil {
		panic(err)
	}*/
}

// Play the minigame as per http://secretsofgrindea.wikia.com/wiki/Fishing.
func playMinigame() () {
	// 1. Simulate a 'Z' keypress.
	pressZ()
	// 2. Wait until we have a fish on the hook.
	// 3. Press 'Z' to start reeling in the fish.
	pressZ()
	// 4. Press the left and right keys to play the minigame.
}

// main starts the fishing process.
func main() {
	// 1. Ensure that Secrets of Grindea is running in the foreground.
	if isGrindeaReady() {
		// 2. Play the minigame.
		playMinigame()
		// 3. Repeat step 2 until cancelled.
	}
}

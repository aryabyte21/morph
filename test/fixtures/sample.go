package main

import "fmt"

func greet(name string) {
	fmt.Println("hi", name)
	if len(name) > 0 {
		fmt.Println(name)
	}
	log.Info("done")
}

func main() {
	greet("world")
	fmt.Println("end")
}

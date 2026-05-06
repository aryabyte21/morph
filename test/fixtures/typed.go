package main

import "fmt"

func log(msg string) {
	fmt.Println(msg)
}

var greeting string = "hello"
var count int = 42

func caller(s string, n int) {
	log(s)
	log("inline")
	log(greeting)
	log(count)
}

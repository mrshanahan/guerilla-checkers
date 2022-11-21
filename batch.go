package main

import (
    "bufio"
    "flag"
    "fmt"
    "os"
    "time"
)

func main() {
    intervalPtr := flag.Int("i", 5, "Interval in seconds to wait between batches")
    flag.Parse()

    interval := *intervalPtr
    if (interval <= 0) {
        fmt.Fprintf(os.Stderr, "error: invalid interval value (expected > 0, got %d)\n", interval)
        os.Exit(1)
    }
    scanner := bufio.NewScanner(os.Stdin)
    lines := make(chan string, 10)
    done := make(chan bool)
    releaseTimer := time.NewTicker(time.Duration(interval) * time.Second)

    go func() {
        complete := false
        for (!complete) {
            <-releaseTimer.C
            sum := 0
            processing := true
            for (processing) {
                select {
                case _ = <-lines:
                    sum += 1
                default:
                    processing = false
                }
            }
            if (sum > 0) {
                // fmt.Println(sum)
                fmt.Printf("%d%c", sum, 0)
            }
            select {
            case _ = <-done:
                complete = true
            default:
                complete = false
            }
        }
    }()

    for scanner.Scan() {
        line := scanner.Text()
        lines <- line
    }
    done <- true
    releaseTimer.Stop()
    close(lines)

    if err := scanner.Err(); err != nil {
        fmt.Fprintln(os.Stderr, "error:", err)
        os.Exit(1)
    }
}

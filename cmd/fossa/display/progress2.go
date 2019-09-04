package display

import (
	"strconv"
	"strings"
	"sync/atomic"
)

// A Display provides progress indicators for running "processes".
//
// Running "process" indicators are joined by spaces, e.g.,
// [] nodejs(Discovery) golang(Analysis 1/5)
//
// See ProgressTracker for examples
type Display interface {
	// Start a new process and start tracking its progress
	// `StartProcess("nodejs")`
	// -> [] nodejs
	StartProcess(processName string) ProgressTracker
	// Stop the progress display. All future updates from processes will be ignored
	Stop()
}

func StartDisplay() Display {
	progressUpdates := make(chan progressUpdate)
	stop := make(chan struct{})

	// TODO: disgusting
	go func() {
		// TODO: linked/sorted map?
		progress := make(map[string]string)

		// TODO: move this elsewhere?
		writeProgress := func() {
			var progressStrings []string
			for k, v := range progress {
				if v == "" {
					progressStrings = append(progressStrings, k)
				} else {
					progressStrings = append(progressStrings, k+"("+v+")")
				}
			}
			InProgress(strings.Join(progressStrings, " "))
		}

		for {
			select {
			case update := <-progressUpdates:
				if update.Ended {
					delete(progress, update.ProcessName)
				} else {
					progress[update.ProcessName] = update.Status
				}
				writeProgress()
				break
			case <-stop:
				return
			}
		}
	}()

	return displayImpl{progressUpdates: progressUpdates, stop: stop}
}

type displayImpl struct { // TODO: properly integrate
	progressUpdates chan progressUpdate
	stop            chan struct{}
}

// A ProgressTracker is created by calling `Display.StartProcess(processName string)`
//
// A ProgressTracker is used to show the current step and progress for a given"process"
// Examples below are using `Display.StartProcess("nodejs")`
//
// `Begin("Discovery")`
// -> [] nodejs(Discovery)
//
// `Begin("Discovery")`
// `AddCompleted(5)`
// -> [] nodejs(Discovery 5/?)
//
// `Begin("Analysis")`
// `AddTasks(5)`
// -> [] nodejs(Analysis 0/5)
// `AddCompleted(1)`
// -> [] nodejs(Analysis 1/5)
//
// `End()`
// -> []
//
// NOTE: only the `AddCompleted` and `AddTasks` functions are safe to be called
// concurrently.
type ProgressTracker interface {
	// Begin a new step in this process
	// Clears the current count of Completed and Tasks
	Begin(stepName string)
	// Update the number of completed actions
	// When called before `AddTasks`, the count of tasks will display as "?"
	AddCompleted(n int)
	// Update the number of tasks that need to be run
	// When called before `AddCompleted`, the count of completed will display as "0"
	AddTasks(n int)
	// End the progress tracker for this process
	//
	// NOTE: When reusing the same ProgressTracker, be sure to call `Begin`
	End()
}

type atomicProgressTracker struct {
	updatesChan chan<- progressUpdate
	processName string
	stepName    *atomic.Value
	completed   *int32
	tasks       *int32
}

func (p atomicProgressTracker) Begin(stepName string) {
	// Clear state from last step
	atomic.StoreInt32(p.completed, -1)
	atomic.StoreInt32(p.tasks, -1)
	p.stepName.Store(stepName)
	p.sendUpdate()
}

func (p atomicProgressTracker) AddCompleted(n int) {
	atomic.CompareAndSwapInt32(p.completed, -1, 0)
	atomic.AddInt32(p.completed, int32(n))
	p.sendUpdate()
}

func (p atomicProgressTracker) AddTasks(n int) {
	atomic.CompareAndSwapInt32(p.tasks, -1, 0)
	atomic.AddInt32(p.tasks, int32(n))
	p.sendUpdate()
}

func (p atomicProgressTracker) sendUpdate() {
	var status string

	step := p.stepName.Load()
	if step != nil {
		status += step.(string)
	}

	tasks := atomic.LoadInt32(p.tasks)
	completed := atomic.LoadInt32(p.completed)
	if tasks != -1 && completed != -1 {
		status += " " + strconv.Itoa(int(completed)) + "/" + strconv.Itoa(int(tasks))
	} else if tasks != -1 {
		status += " 0/" + strconv.Itoa(int(tasks))
	} else if completed != -1 {
		status += " " + strconv.Itoa(int(completed)) + "/?"
	}

	update := progressUpdate{
		ProcessName: p.processName,
		Status:      status,
		Ended:       false,
	}

	select {
	case p.updatesChan <- update:
	default:
	}
}

func (p atomicProgressTracker) End() {
	p.updatesChan <- progressUpdate{
		ProcessName: p.processName,
		Status:      "",
		Ended:       true,
	}
}

func (d displayImpl) StartProcess(processName string) ProgressTracker {
	completed := int32(-1)
	tasks := int32(-1)

	// Don't block on chan
	update := progressUpdate{
		ProcessName: processName,
		Status:      "",
		Ended:       false,
	}
	select {
	case d.progressUpdates <- update:
	default:
	}
	return atomicProgressTracker{updatesChan: d.progressUpdates, processName: processName, completed: &completed, tasks: &tasks, stepName: &atomic.Value{}}
}

func (d displayImpl) Stop() {
	close(d.stop)
}

type progressUpdate struct {
	ProcessName string
	Status      string
	Ended       bool
}

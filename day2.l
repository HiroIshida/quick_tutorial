## Day2: control simulated robot 
Before starting this tutorial, please run
```
roslaunch quick_tutorial common.launch
```
in other terminal. You will get the gazebo window as: 
<div align="center">
<img src="https://raw.githubusercontent.com/HiroIshida/quick_tutorial/master/images/day2_1.png" alt="none" title="day2_1" width="300">
</div>

### send command workflow
```
(load "package://fetcheus/fetch-interface.l")
(require "models/arrow-object.l")
(fetch-init)

(setq *co-handle* (arrow))
(send *co-handle* :newcoords (make-coords :pos #f(500 600 300) :rpy #f(0 0.6 1.54)))


(defun show-all ()
  (let ((co-endeffector (arrow)))
    (send co-endeffector :newcoords (send (send *fetch* :rarm :end-coords) :copy-worldcoords))
    (objects (list co-endeffector *co-handle* *fetch*))))

(setq *co-ik-target* (send *co-handle* :copy-worldcoords))
(send *co-ik-target* :translate #f(-150 0 0) :local)

(send *fetch* :rarm :inverse-kinematics *co-ik-target*
        :rotation-axis t :check-collision t :use-torso t)

(send *ri* :angle-vector-raw (send *fetch* :angle-vector) 1000)
(send *ri* :wait-interpolation)

(send *fetch* :rarm :move-end-pos #f(180 0 0) :local)
(send *ri* :angle-vector-raw (send *fetch* :angle-vector) 1000)
(send *ri* :wait-interpolation)
(send *ri* :start-grasp :effort 1000)

;; hook
(send *fetch* :rarm :move-end-pos #f(0 0 100) :world)
(send *fetch* :rarm :move-end-rot 40 :z :local)
(send *ri* :angle-vector-raw (send *fetch* :angle-vector) 1000)
(send *ri* :wait-interpolation)

;; lift
(send *fetch* :rarm :move-end-pos #f(0 0 700) :world :use-torso t)
(send *ri* :angle-vector-raw (send *fetch* :angle-vector) 1000)
(send *ri* :wait-interpolation)

;; go
(send *ri* :go-pos-unsafe 1.0 -0.5 -0.4)

;; down
(send *fetch* :rarm :move-end-pos #f(0 0 -80) :world)
(send *ri* :angle-vector-raw (send *fetch* :angle-vector) 1000)
(send *ri* :stop-grasp)
(send *ri* :wait-interpolation)

;; withdraw
(send *fetch* :rarm :move-end-pos #f(-150 0 0) :local)
(send *ri* :angle-vector-raw (send *fetch* :angle-vector) 1000)
(send *ri* :wait-interpolation)


```


### TODO 
how to reset world




## Day2: control simulated robot 
<div align="center">
<img src="https://raw.githubusercontent.com/HiroIshida/quick_tutorial/master/images/day2_whole.gif" alt="none" title="day2_1" width="400">
</div>
### we call real robot.
Before starting this tutorial, please run
```
roslaunch quick_tutorial common.launch
```
in other terminal. 

### state potentio-vectro
### send command workflow
As a preparation load some libraries and call `fetch-init` function.
```
(load "package://fetcheus/fetch-interface.l")
(require "models/arrow-object.l")
(fetch-init)
```
Different from `fetch` function that we called in the [previous demo](link), `fetch-init` create robot interface object `*ri*` as well as `*fetch*`. Through *ri*, we can control a real (or simulated) robot and obtain state of the real robot. Note that joint angles of `*fetch*` is set such that it's same as the real one. Please call `(objects *fetch*)` and compare posture of `*fetch*` with the real robot.     
Suppose, you want to guide the end-effector to a specific coordinate `*co-handle*`. 
```
(setq *co-handle* (arrow))
(send *co-handle* :newcoords (make-coords :pos #f(800 300 1000) :rpy #f(0.0 0.0 1.54)))
```
Now let's visualize the `*fetch*`, `*co-handle*` and coordinate of the end-effector all togather:
```
(setq *co-endeffector* (arrow))
(send *co-endeffector* :newcoords (send (send *fetch* :rarm :end-coords) :copy-worldcoords))
(objects (list *fetch* *co-endeffector* *co-handle*))
```

Rather than directory guide the end-effector to `*co-handle*`, let's guide to a coordinate `*co-ik-target*` which is slightly behind the target:
```
(setq *co-ik-target* (send *co-handle* :copy-worldcoords))
(send *co-ik-target* :translate #f(-80 0 0) :local)
```
Because we set `:local`, the translation is done w.r.t. `*co-ik-target*` itself. Not that you can also set `:world`. By calling `(objects (list *fetch* *co-endeffector* *co-handle* *co-ik-target*))` you will see the following figure.

Now we solve the IK,
```
(send *fetch* :rarm :inverse-kinematics *co-ik-target*
        :rotation-axis t :check-collision t :use-torso t)
```
and send it to the real robot!
```
(send *ri* :angle-vector (send *fetch* :angle-vector) 1000) ;; command 1
(send *ri* :wait-interpolation)
```
Here by specifying `1000`, it takes 1000 msec for the real robot to follow the commanded angle-vecttor. Note that you cannot specify smaller value than 1000. 

Then move the end-effector forward by
```
(send *fetch* :rarm :move-end-pos #f(100 0 0) :local)
(send *ri* :angle-vector (send *fetch* :angle-vector) 1000) ;; command 2
(send *ri* :wait-interpolation)
```
In the `:move-end-pos` method, IK is solved. Similar to `:translate` you can also specify `:world` instead of `:local`. `:wait-interpolation` is important. Without callint this, commanded angle-vector is overwritten. For example, if you send commands 1 and 2 above without `:wait-interpolation` it is almost same as sending only `command2`. (explain why I said *almost* somewhere).

Grasp the handle with effort of 100:
```
(send *ri* :start-grasp :effort 100)
```
Lift it:
```
(send *fetch* :rarm :move-end-pos #f(0 0 100) :world)
(send *ri* :angle-vector (send *fetch* :angle-vector) 1000)
(send *ri* :wait-interpolation)
```
As you probably noticed, basic workflow is repetition of 1) solveing IK in Euslisp side (and check the geometry) and then 2) command it to the real robot by `:angle-vector` method.

## おちないようにかたむける しあげとして
### TODO 
rot of coordinate how to





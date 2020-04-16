## Day1: playing geometric robot model in Euslisp
Before startgin this tutorial, please do `roscore` in other terminal. 

### get information of joint angles of a robot

``` 
(load "package://fetcheus/fetch-interface.l") 
(fetch) 
```
The `fetch` function is create an instance named `*fetch*` which is a geometrical model of fetch-robot. Now you can view the model by calling the following function:
```
(objects *fetch*)
```
<div align="center">
<img src="https://raw.githubusercontent.com/HiroIshida/quick_tutorial/master/images/day1_1.png" alt="none" title="day1_1" width="200">
</div>

The robot model `*fetch*` contains the information of joints. The fetch robot has 10 joints, so let's look at state of these joints.
```
(send *fetch* :angle-vector)
;; output: #f(20.0 75.6304 80.2141 -11.4592 98.5487 0.0 95.111 0.0 0.0 0.0)
```
As you can see, the state of 10 joints are shown as a `float-vector`. Probably you need to know which value corresponds to which joints. To this end, the following method is useful.
```
(send *fetch* :joint-list :name)
;; output: ("torso_lift_joint" "shoulder_pan_joint" "shoulder_lift_joint" "upperarm_roll_joint" "elbow_flex_joint" "forearm_roll_joint" "wrist_flex_joint" "wrist_roll_joint" "head_pan_joint" "head_tilt_joint")
```
You get the list of 10 strings of joint name. The order of this list corresponds to the order of the float-vector you got by `:angle-vector` method. By comparing those two, for example, you know that `torso_lift_joint` has angle of `20.0`.

Now, let's set a custom angle vector to the robot model. 
```
(setq *av-zero* (float-vector 0 0 0 0 0 0 0 0 0 0))
(send *fetch* :angle-vector *av-zero*)
```
Please click the IRT-viewer previously opend by `objects` function, then the you will see the robot model is updated. 
<div align="center">
<img src="https://raw.githubusercontent.com/HiroIshida/quick_tutorial/master/images/day1_2.png" alt="none" title="day1_2" width="200">
</div>


Maybe, you want to set specific joint instead of set all the joints angle at once. For the `shoulder_pan_joint` case for example, this can be done by:
```
(let ((shoulder-pan-joint (send *fetch* :shoulder_pan_joint)))
    (send shoulder-pan-joint :joint-angle 60))
```
Note that the same thing can be done by `(setq *fetch* :shoulder_pan_joint :joint-angle 60)`, which is more common in jsk. You will get following image:
<div align="center">
<img src="https://raw.githubusercontent.com/HiroIshida/quick_tutorial/master/images/day1_3.png" alt="none" title="day1_3" width="200">
</div>
You will observe that only the state of the single joint is changed by compareing this figure and previous one.

### solving inverse kinematics (IK)
Usually, in robotics, you want to guide the robot arm's end effector to a commanded pose (position and orientation). Thus, before sending an angle vector, you must know an angle vector with which the end effector will be the commanded pose. This can be done by solving inverse kinematics (IK) (if you are not familiar please google it). First, we create a coordinate (or a pose) `*co*` by
```
(setq *co* (make-coords :pos #f(800 300 800) :rpy #f(0.0 0.1 0.1))) ;; #f(..) is a float-vector
```
Then the following code will solver the IK:
```
(send *fetch* :angle-vector #f(0 0 0 0 0 0 0 0 0 0))
(send *fetch* :rarm :inverse-kinematics *co*
        :rotation-axis t :check-collision t :use-torso nil)
```
<div align="center">
<img src="https://raw.githubusercontent.com/HiroIshida/quick_tutorial/master/images/day1_4.png" alt="none" title="day1_4" width="200">
</div>

The above function first solved IK inside to obtain the angle-vector such that the coordinate of the end-effector equals to ` *co* `. Then set the obtained angle-vector to ` *fetch* `. (I personally think this function is bit strange and conufsing. (For me it is more straightforward if the function return just sangle-vector without setting it `*fetch*`.) Note that you must care initial solution for IK. In the euslisp the angle-vector set to the robot is used as the initial solution. For example, in the above code I set it to " #f(0 0 0 0 0 0 0 0 0 0)" . In solving IK you can set some key arguments. `:rotation-axis`, `check-collision` and `use-toros` is particurally important. If `:rotation-axis` is `nil` the IK is solved ignoreing orientation (rpy). If `:check-collision` is `nil` the collision between the links of robot is not considered. Please play with changing these arguments. 

Noting that iterative optimization takes place in solving IK, the solution will be change if you change the initial solution. Let's try with different initial solution:
```
(send *fetch* :angle-vector #f(20.0 75.6304 80.2141 -11.4592 98.5487 0.0 95.111 0.0 0.0 0.0))
(send *fetch* :rarm :inverse-kinematics *co*
        :rotation-axis t :check-collision t :use-torso nil)
```
Now you will see different solution is obtained from the previous one.
<div align="center">
<img src="https://raw.githubusercontent.com/HiroIshida/quick_tutorial/master/images/day1_5.png" alt="none" title="day1_5" width="200">
</div>

Now let's check that actually inverse kinematics is solved by displaying `*co*` and the coordinate of end-effector `*co-endeffector*`.
```
(setq *co-endeffector* (send (send *fetch* :rarm :end-coords) :copy-worldcoords)) 
(objects (list *fetch* *co* *co-endeffector*))
```

<div align="center">
<img src="https://raw.githubusercontent.com/HiroIshida/quick_tutorial/master/images/day1_6.png" alt="none" title="day1_6" width="200">
</div>
You can see the two coordinates (diplayed by white arrows) are equal to each other.

### move-end-pos
under construction

### better visualization by arrow-object 
under constuction
```
(require "models/arrow-object.l")
(setq *object-coords* (arrow))
(send *object-coords* :newcoords (make-coords :pos object-pos :rpy #f(0 0 0)))
```


## introduction to robot model
before startgin this tutorial, please do `roscore` in other terminal.

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
<img src="https://raw.githubusercontent.com/HiroIshida/quick_tutorial/master/images/day1_2.png" alt="none" title="day1_3" width="200">
</div>
You will observe that only the state of the single joint is changed by compareing this figure and previous one.

### solving inverse kinematics
Usually, in robotics, you want to guide the robot arm's end effector to a commanded pose (position and orientation). Thus, before sending an angle vector, you must know an angle vector with which the end effector will be the commanded pose. This can be done by solving inverse kinematics (if you are not familiar please google it). First, we create a coordinate (or a pose) `*co*` by
``
(setq *co* (make-coords :pos #f(800 0 1000) :rpy #f(0.0 0.3 1.54))) ;; #f(..) is a float-vector
```
Then the following code will solver the inverse kinematics.
```
(send *fetch* :rarm :inverse-kinematics
        (send *co-object* :copy-worldcoords)
        :rotation-axis t :check-collision t :use-torso nil)
```
Now you can see







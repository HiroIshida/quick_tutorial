### Quick Tutorial
This package provides quick tutorials in a step-by-step manner. In [day1](day1.md), you will get to know how to kinematically simulate fetch robot inside Euslisp. In [day2](day2.md), you will be familiar with the workflow for controlling a real robot using Euslisp. In day3 (under construction), you will know how to implement simple sense-and-act type robotic manipulation.

### Installation
We assume you already installed ROS melodic.

```bash
mkdir ~/tutorial_ws/src -p
cd ~/tutorial_ws/src

git clone https://github.com/jsk-ros-pkg/jsk_robot.git
git clone https://github.com/jsk-ros-pkg/jsk_demos.git
git clone https://github.com/HiroIshida/quick_tutorial.git
rosdep install --from-paths jsk_robot/jsk_fetch_robot/fetcheus -i -r -y
rosdep install --from-paths jsk_demos/jsk_maps -i -r -y
rosdep install --from-paths quick_tutorial -i -r -y

cd ..
source /opt/ros/melodic/setup.bash
catkin build fetcheus jsk_maps quick_tutorial
source ~/tutorial_ws/devel/setup.bash
```

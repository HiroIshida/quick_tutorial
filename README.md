### Preparation

```bash
mkdir -p ~/tutorial_ws/src
cd ~/tutorial_ws/src
git clone https://github.com/jsk-ros-pkg/jsk_robot.git
cd jsk_robot/jsk_fetch_robot
rosdep install fetcheus -i -y -r
cd ~/tutorial_ws 
catkin build
source devel/setup.bash
```


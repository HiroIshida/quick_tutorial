### Preparation

```bash
mkdir ~/catkin_ws/src -p
cd ~/catkin_ws/src

git clone https://github.com/jsk-ros-pkg/jsk_demos.git &
git clone https://github.com/HiroIshida/quick_tutorial.git &
git clone https://github.com/708yamaguchi/jsk_robot.git

cd jsk_robot
git checkout 368276697279f86ba42e3ba5854a87241421fd65
cd ..

rosdep install --from-paths jsk_robot jsk_demos/jsk_maps --ignore-src -y -r

cd ..
source /opt/ros/melodic/setup.bash
catkin build fetcheus quick_tutorial jsk_maps
```


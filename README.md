### Instlation (same as yamaguchi demo)
```bash
mkdir -p catkin_ws/src
cd catkin_ws/src
git clone https://github.com/708yamaguchi/fetch_gazebo.git
cd fetch_gazebo
git checkout -t origin/demo-melodic
cd ..
rosdep install --from-paths . --ignore-src -y -r
cd ..
catkin build
source devel/setup.bash
```


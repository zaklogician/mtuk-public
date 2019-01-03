#/bin/sh
echo "1/3 Cleaning..."
rm -r output/*

echo "2/3 Building Elm program..."
cd src
elm-make Main.elm --output=../output/main.js

echo "3/3 Copying static data..."
cp -r ../html/* ../output

echo "Done!"

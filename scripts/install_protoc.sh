curl -# -O https://protobuf.googlecode.com/files/protobuf-2.5.0.tar.gz
gunzip protobuf-2.5.0.tar.gz
tar -xvf protobuf-2.5.0.tar
cd protobuf-2.5.0
./configure --prefix=/usr
make
make install
sudo ldconfig
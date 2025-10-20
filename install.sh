conda create -n mbo_config --clone r-4.4
conda activate mbo_config
conda install eigen libxml2 pkg-config zlib libpng xz

export PKG_CONFIG_PATH=/glade/work/marcbecker/conda-envs/mbo_config/lib/pkgconfig/
export LD_LIBRARY_PATH=/glade/work/marcbecker/conda-envs/mbo_config/lib:$LD_LIBRARY_PATH

# hyperqueue
git clone https://github.com/hyperqueue/hyperqueue.git
cd hyperqueue
RUSTFLAGS="-C target-cpu=native" cargo build --release --no-default-features
export PATH=/glade/work/marcbecker/hyperqueue/target/release:$PATH

default: output

build:
	time ~/.cabal/bin/bnfc --haskell -o src Lambda.bnfc
	rm src/TestLambda.hs
	time stack build
	time stack test

qemu:
	qemu-system-riscv64 \
		-nographic \
		-machine virt \
		-smp 4 \
		-m 2G \
		-kernel risc-v-fedora/Fedora-Minimal-Rawhide-*-fw_payload-uboot-qemu-virt-smode.elf \
		-bios none \
		-object rng-random,filename=/dev/urandom,id=rng0 \
		-device virtio-rng-device,rng=rng0 \
		-device virtio-blk-device,drive=hd0 \
		-drive file=risc-v-fedora/Fedora-Minimal-Rawhide-20200108.n.0-sda.raw,format=raw,id=hd0 \
		-device virtio-net-device,netdev=usernet \
		-netdev user,id=usernet,hostfwd=tcp::10000-:22	

output: build
	rm -f test_data/assembly/* test_data/assembly_out/* test_data/interpret_out/*
	for testName in $(ls test_data/lambda_code); do echo $testName; stack exec Mary-exe $testName ; done
	time sshpass -p "fedora_rocks!" scp -r test_data/assembly scp://riscv@localhost:10000
	time sshpass -p "fedora_rocks!" scp test_data/script.sh scp://riscv@localhost:10000
	time echo "./script.sh" | sshpass -p "fedora_rocks!" ssh ssh://riscv@localhost:10000 "bash -s"
	time sshpass -p "fedora_rocks!" scp -r scp://riscv@localhost:10000/assembly_out test_data/
	diff -s test_data/interpret_out/ test_data/assembly_out/

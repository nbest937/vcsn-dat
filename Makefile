.PHONY: scp go

scp:
	scp skadi.uchicago.edu:~/raid/crop_climate_impacts/new_zealand/Daily_VCSN_data/vcsn_1972.zip .
	unzip vcsn_1972.zip 19720101_vcsn.dat

go:
	ssh -t cli.globusonline.org endpoint-activate ci#pads
	ssh -t cli.globusonline.org endpoint-activate nbest#nbest
	ssh cli.globusonline.org scp ci#pads:/gpfs/pads/projects/see/data/raw/new_zealand/Daily_VCSN_data 
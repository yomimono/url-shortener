location := "url-shortener-db.img"
host := "10.0.0.2"
show_status := "-w 'status code: %{http_code}'"

initiate :
	dd if=/dev/zero of={{location}} bs=1M count=1
	format --block-size 512 {{location}}

start :
	sudo solo5-hvt --net:service=tap100 --block:shortener={{location}} -- shortener.hvt --backtrace=true -l "application:debug"

test :
	curl {{show_status}} -q -s -S {{host}} |grep 200
	curl -q -s -S {{host}}/uptime |grep "since"
	curl --data short_name=yomimono\&url="https://somerandomidiot.com" {{host}}
	curl --verbose {{host}}/yomimono 2>&1 |grep somerandomidiot
	curl {{show_status}} {{host}}/yomimono 2>&1 |grep 307
	curl --data short_name=yomimono\&url="https://somerandomidiot.com" {{host}} | grep "already a URL"

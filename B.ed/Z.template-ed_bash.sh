#!/bin/bash
#SBATCH -A FORTEPROJECT
#SBATCH -t 0-10:00:00
#SBATCH -N 1


start_time="$(date -u +%s)"

bash /people/dorh012/to/the/run_ed.sh


end_time="$(date -u +%s)"

elapsed="$(bc <<<"$end_time-$start_time")"
echo "Total of $elapsed seconds elapsed for process"

done

wait

echo "Done!"


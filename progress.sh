#!/bin/bash
chapters=(46 97 82 79 52)
printf "Chapter   Problems   Completed   Percentage\n"

for((i=0 ; i<5 ; i++)); {
    completed=$(find ./chap-$(($i + 1)) -name *.ss | wc -l)
    printf "    $(($i + 1))          ${chapters[$i]}          $completed     $(($(($completed * 100)) / ${chapters[$i]})) %%\n"
    total=$(($total + ${chapters[$i]}))
    total_completed=$(($total_completed + $completed))
}

echo "------------------------------------------"
printf "  Total       $total        $total_completed      $(($(($total_completed * 100)) / $total)) %%\n"
echo "------------------------------------------"
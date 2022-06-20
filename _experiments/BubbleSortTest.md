---
title: Bubble Sort Test
layout: lab-experiment
jsfile: ../js/bubbleSortTest.js
elm: BubbleSortTest
order: 0
---


# Objective
Apply Bubble Sort Algorithm on the given array to sort it using the provided controls.
Please note that:
1. You don't apply any optimizations over original algorithm.
2. Sorting of the array is the secondary objective.The primary objective is correct application of bubble sort algorithm.

# Variables Description

| Variable | Data Type | Valid values | Initialization |
| :------: | :-------: | :-----------:| :-------------:|
| i        | int       | [0,n)        | 0              |
|----------+-----------+--------------+----------------|
| b        | int       | [0,n]        | n              |

where n is the length of the array.

# Controls Description

| Control                 | Description                                                         |
| :---------------------: | :-----------------------------------------------------------------: |
| Increment i             | Increments i by 1, if i < n - 1                                     |
|-------------------------+---------------------------------------------------------------------|
| Swap And Increment i    | Swaps array[i] and array[i+1], and increments i by 1, if i < n - 1  |
|-------------------------+---------------------------------------------------------------------|
| Decrement b And Reset i | Decrements b by 1 and resets i to 0, if b > 0                       |

# Procedure
1. Click on suitable control to simulate next step of bubble sort algorithm. 
2. You can do Undo,Redo,Reset by clicking on the respective buttons as per your need. 
3. Click on Submit button when you are done.
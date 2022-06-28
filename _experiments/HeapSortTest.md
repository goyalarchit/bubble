---
title: Heap Sort Test
layout: lab-experiment
jsfile: ../js/heapSortTest.js
elm: HeapSortTest
order: 2
---


# Objective
Apply Heap Sort Algorithm on the given Max Heap to sort the underlying array represented by the Complete Binary Tree using the provided controls.
Please note that:
1. You should not apply any optimizations over the original algorithm i.e simulate the heap sort algorithm to completion.
2. Sorting of the array is the secondary objective. The primary objective is correct application of heap sort algorithm.
# Variables Description

| Variable | Data Type | Valid values | Initialization      |
| :------: | :-------: | :-----------:| :-----------------: |
| i        | int       | [0,n)        | 0(root node)        |
|----------+-----------+--------------+-------------------- |
| b        | int       | [0,n)        | n-1(last node)      |

where n is the length of the array.



# Controls Description

| Control                    | Description                                                         |
| :-------------------------:| :-----------------------------------------------------------------: |
| Swap And Move (LeftChild)  |  If left child of node i exists:                                    |
|                            |       1. swaps the value of node i and its left child.             | 
|                            |       2. sets i to point to its left child.                        |
|----------------------------+---------------------------------------------------------------------|
| Swap And Move (RightChild) |  If right child of node i exists:                                   |
|                            |       1. swaps the value of node i and its right child.            | 
|                            |       2. sets i to point to its right child.                       |
|----------------------------+---------------------------------------------------------------------|
| Reset i                    | Resets i to point to root node .                                     |
|----------------------------+---------------------------------------------------------------------|
| Swap i,b And Move (PreviousNode)        |  If there exists a node before node b in level order traversal of the tree :|
|                            |       1. swap the values of node i and node b.                           | 
|                            |       2. set b to point to its previous node.                           |

# Procedure
1. Click on suitable control to simulate next step of Heap Sort algorithm. 
2. You can do Undo,Redo,Reset by clicking on the respective buttons as per your need. 
3. Click on Submit button when you are done.
---
title: Heapify Test
layout: lab-experiment
jsfile: ../js/heapifyTest.js
elm: HeapifyTest
order: 1
---


# Objective
Apply Heapify Algorithm on the given array to convert it to Max Heap using the provided controls. Array is represented as a Complete Binary Tree(CBT).

# Variables Description

| Variable | Data Type | Valid values | Initialization      |
| :------: | :-------: | :-----------:| :-----------------: |
| i        | int       | [0,n)        | n//2-1(last parent) |
|----------+-----------+--------------+-------------------- |
| p        | int       | [0,n)        | n//2-1(last parent) |

where n is the length of the array.



# Controls Description

| Control                    | Description                                                         |
| :-------------------------:| :-----------------------------------------------------------------: |
| Swap And Move (LeftChild)  |  If left child of node i exists:                                    |
|                            |       1. swaps the value of node i and it's left child.             | 
|                            |       2. sets i to point to it's left child.                        |
|----------------------------+---------------------------------------------------------------------|
| Swap And Move (RightChild) |  If right child of node i exists:                                   |
|                            |       1. swaps the value of node i and it's right child.            | 
|                            |       2. sets i to point to it's right child.                       |
|----------------------------+---------------------------------------------------------------------|
| Move (PreviousNode)        |  If there exists a node before node p in level order traversal of the tree :|
|                            |       1. set i to point to the previous node.                            | 
|                            |       2. set p to point to the previous node.                           |

# Procedure
1. Click on suitable control to simulate next step of heapify algorithm. 
2. You can do Undo,Redo,Reset by clicking on the respective buttons as per your need. 
3. Click on Submit button when you are done.
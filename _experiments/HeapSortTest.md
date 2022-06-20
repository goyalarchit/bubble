---
title: Heap Sort Test
layout: lab-experiment
jsfile: ../js/heapSortTest.js
elm: HeapSortTest
order: 2
---


# Objective
Apply Heap Sort Algorithm on the given Max Heap to sort the underlying array using the provided controls.

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
|                            |       1. swaps the value of node i and it's left child.             | 
|                            |       2. sets i to point to it's left child.                        |
|----------------------------+---------------------------------------------------------------------|
| Swap And Move (RightChild) |  If right child of node i exists:                                   |
|                            |       1. swaps the value of node i and it's right child.            | 
|                            |       2. sets i to point to it's right child.                       |
|----------------------------+---------------------------------------------------------------------|
| Reset i                    | Resets i to point to root node .                                     |
|----------------------------+---------------------------------------------------------------------|
| Swap i,b And Move (PreviousNode)        |  If there exists a node before node b in level order traversal of the tree :|
|                            |       1. swap the values of node i and node b.                           | 
|                            |       2. set b to point to it's previous node.                           |

# Procedure
1. Click on suitable control to simulate next step of Heap Sort algorithm. 
2. You can do Undo,Redo,Reset by clicking on the respective buttons as per your need. 
3. Click on Submit button when you are done.
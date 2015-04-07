# quperl_octree2

manage arbitrary volumes of space within octree. The volumes are maintained as 
sets of z-curve intervals. 

A volume is considered to be a connected area of three dimensional space within
the outer bounding box encompassed by the entirety of the octree. The outer 
length for each axis is assumed to be normalized to the unit length (1.0). Thus
all positions are given as three values v with 0.0 =< v < 1.0.  
(TODO: helper function to convert)

Internally the axis are partitioned into 2^(MaxDepth-1) intervals.    

## Usage

Use one of the <code>new</code> functions to create a space:

    $ Empty = quperl_octree:new().
    
    $ AaBB = quperl_octree:new({0.1,0.2,0.3},{0.4,0.5,0.6}).

perform operations on the node-set:

    $ Combined = quperl_octree:merge(Set1, Set2).

retrieve nodes that are in one, but not in the other set: 

    $ Remainder = quperl_octree:subtract(LargeSet, SmallerSet).


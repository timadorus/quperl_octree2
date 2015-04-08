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

Use one of the <code>new_volume</code> functions to create a volume:

    $ Empty = quperl_octree:new_volume().

    $ AaBB = quperl_octree:new_volume({0.1,0.2,0.3},{0.4,0.5,0.6}).

single nodes can be created with:

    $ RootNode = quperl_octree:new_node().

    $ Node = quperl_octree:new_node({0.1,0.2,0.3})

perform operations on the node sets:

    $ Combined = quperl_octree:merge(Volume1, Volume2).

retrieve nodes that are in one, but not in the other set: 

    $ Remainder = quperl_octree:subtract(LargeVolume, SmallerVolume).
    
execute function on each node in a volume:

    $ quperl_octree:map_nodes(Fun, Acc0, Volume).


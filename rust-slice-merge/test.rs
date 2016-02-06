use std::marker::PhantomData;

//TODO merge_left/merge_right/split_left/split_right for lifetimes
//TODO existentials: https://github.com/bluss/indexing

//This is a transparent wrapper over a slice, with two run-time type tags for the beginning and the
//end of the slice
pub struct SliceFragment<'a, T: 'a, A, B>(
    pub &'a mut [T],
    PhantomData<A>,
    PhantomData<B>
);

impl<'a, T: 'a + Sized, A, B> SliceFragment<'a, T, A, B> {

    //Creating an empty slice fragment, for demonstration purposes.
    //Note that the beginning and end markers are the same.
    pub fn empty(slice: &'a mut [T; 0]) -> SliceFragment<'a, T, A, A> {
        SliceFragment(
            slice,
            PhantomData,
            PhantomData
        )
    }
    //Note that the type parameters A and B are user-provided; if they are the same, type safety
    //goes out the window. A better version would use existential types.
    pub fn new(slice: &'a mut [T]) -> SliceFragment<'a, T, A, B> {
        SliceFragment(
            slice,
            PhantomData,
            PhantomData
        )
    }

}

//Merge is only allowed if the end marker of the first slice fragment coincides with the begin
//marker of the second slice fragment
pub fn merge<'a, 'b, 'c: 'a + 'b, T: 'a, A, B, C>(
    SliceFragment(slice1, begin1, _): SliceFragment<'a, T, A, B>,
    SliceFragment(slice2, _, end2)  : SliceFragment<'b, T, B, C>
)  -> SliceFragment<'c, T, A, C> {
    unsafe {
        let slice = std::slice::from_raw_parts_mut(slice1.as_mut_ptr(), slice1.len() + slice2.len());
        SliceFragment(slice, begin1, end2)
    }
}

//NOT TYPE SAFE if any of A, B or C are the same type
pub fn split<'a, T: 'a, A, B, C>(
    SliceFragment(slice, begin1, end2): SliceFragment<'a, T, A, C>,
    index: usize
) -> (SliceFragment<'a, T, A, B>, SliceFragment<'a, T, B, C>) {
    let end1: PhantomData<B>   = PhantomData;
    let begin2: PhantomData<B> = PhantomData;  
    let (slice1, slice2) = slice.split_at_mut(index);
    (
        SliceFragment(slice1, begin1, end1),
        SliceFragment(slice2, begin2, end2)
    )
}

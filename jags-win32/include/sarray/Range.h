#ifndef RANGE_H_
#define RANGE_H_

#include <string>
#include <vector>

/**
 * @short Represents a range of array indices 
 *
 * A Range object represents a range of indices used to take a subset
 * of a multi-dimensional array. For example, A[1:2,4,4:5] is a subset
 * of the array A defined by a range with lower boundary (1,4,4) and
 * upper boundary (2,4,5).
 */
class Range {
    std::vector<int> _lower, _upper;
    std::vector<unsigned int> _dim, _dim_dropped;
    unsigned int _length;
public:
    /**
     * Default constructor which constructs a NULL range, with zero-length
     * upper and lower limits.
     */
    Range();
    /**
     * Constructs a range based on given lower and upper limits
     * A logic_error is thrown if these are of different lengths
     *
     * @param lower Lower limits. 
     *
     * @param upper Upper limits. A range_error is thrown if any
     *              element of upper is smaller than the corresponding
     *              element of lower.
     *
     * @exception range_error
     */
    Range(std::vector<int> const &lower, std::vector<int> const &upper);
    /**
     * Constructs a scalar range from an index. The upper and lower
     * bounds are both equal to the supplied index.  
     */
    Range(std::vector<int> const &index);
    /**
     * Constructs a range from a dimension. For each index, the lower
     * limit is 1 and  the upper limit is the corresponding element of
     * dim (cast to a signed int).
     *
     * This constructor should not be confused with the constructor
     * that creates a scalar range from a vector of signed integers.
     */
    Range(std::vector<unsigned int> const &dim);
    /**
     *Equality operator
     */
    bool operator==(Range const &range) const;
    /**
     * Inequality operator 
     */
    bool operator!=(Range const &range) const;
    /**
     * Length of the range. This is the number of indices that are
     * contained in the range. For example, the range [1:2,4,4:5]
     * contains 4 elements.
     */
    unsigned int length() const;
    /**
     * Indicates whether the test range is completely contained inside this
     * range.
     *
     * @param test_range Test range, which must have the correct number of
     * dimensions, or an invalid_argument exception is thrown.  
     *
     * @exception invalid_argument
     */
    bool contains(Range const &test_range) const;
    /**
     * Returns the value of a RangeIterator constructed from this range,
     * after n iterations of RangeIterator#nextLeft 
     *
     * @see RangeIterator
     */
    std::vector<int> leftIndex(unsigned int n) const;
    /**
     * The inverse of leftIndex. Returns the number of iterations of 
     * RangeIterator#nextLeft required to reach the given index.
     *
     * @param index Index vector to convert to offset. An out_of_range
     * exception is thrown if the index is not contained in the range.
     * @see RangeIterator
     * @exception out_of_range
     */
    unsigned int leftOffset(std::vector<int> const &index) const;
    /**
     * Returns the value of a RangeIterator after n iterations of
     * RangeIterator#nextRight 
     *
     * @see RangeIterator
     */
    std::vector<int> rightIndex(unsigned int n) const;
    /**
     * The inverse of rightIndex. Returns the number of iterations of 
     * RangeIterator#nextRight required to reach index.
     *
     * @param index Index vector to convert to offset. An out_of_range
     * exception is thrown if the index is not contained in the range.  
     * @see RangeIterator
     * @exception out_of_range
     */
    unsigned int rightOffset(std::vector<int> const &index) const;
    /**
     * Dimension of the range. The range [1:4,2,3:5] has dimension
     * (4,1,3) if drop==false and (4,3) if drop==true. Dropping of
     * dimensions follows the S language convention.
     *
     * @param drop Should dimensions of size 1 be dropped? 
     */
    std::vector<unsigned int> const &dim(bool drop) const;
    /**
     * Number of dimensions covered by the Range
     *
     * @param drop Should dimensions of size 1 be counted?
     */
    unsigned int ndim(bool drop) const;
    /**
     * lower limit of range
     */
    std::vector<int> const & lower() const;
    /**
     * upper limit of range
     */
    std::vector<int> const & upper() const;
    /**
     * Less than operator based on lexicographic ordering of the
     * upper bound, then the lower bound.
     */
    bool operator<(Range const &rhs) const;
};

/**
 * Tests for NULL ranges.
 */
inline bool isNULL(Range const &range) { return range.length() == 0; }

/**
 * Returns a string containing a BUGS language representation of the
 * given range: e.g. a range with lower limit (1,2,3) and upper limit
 * (3,3,3) will be printed as "[1:3,2:3,3]"
 */
std::string print(Range const &range);

#endif /* RANGE_H_ */

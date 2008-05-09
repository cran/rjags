#ifndef SARRAY_H_
#define SARRAY_H_

#include <sarray/Range.h>

#include <vector>
#include <string>

/**
 * @short multi-dimensional real-valued array
 *
 * An SArray represents a multi-dimensional array of double precision values.
 */
class SArray 
{
    Range const _range;
    std::vector<double> _value;
    bool _discrete;
    std::vector<std::string> _dimnames;
    SArray &operator=(SArray const &rhs);
public:
    /**
     * Constructor for SArrays. 
     * 
     * On construction, the elements of the value array are all equal to
     * JAGS_NA.  
     * 
     * @param dim Dimension of SArray to be constructed
     */
    SArray(std::vector<unsigned int> const &dim);
    /**
     * Copy constructor.
     *
     * Note that the assignment operator of the SArray class is
     * declared private, and so cannot be used.  An SArray can only be
     * copied by a constructor. This ensures that, once constructed,
     * an SArray cannot change its length or dimension.
     */
    SArray(SArray const &orig);
    /**
     * Sets the value of an SArray.
     *
     * @param value vector of values to be assigned. The size of this
     * vector must match the length of the SArray or a length_error
     * exception will be thrown.
     *
     * @exception length_error
     */
    void setValue(std::vector<double> const &value);
    /**
     * Sets the value of an SArray to an integer vector
     */
    void setValue(std::vector<int> const &value);
    /**
     * Sets the value of a single element of SArray
     *
     * @param value Scalar value to be assigned
     *
     * @param offset Distance along the value array
     */
    void setValue(double value, unsigned int offset);
    /**
     * The value of the SArray . 
     *
     * Values are given in column-major order (i.e. with the left hand
     * index moving fastest), like the S language and Fortran.
     *
     * @return A reference to a vector of values.
     */
    std::vector<double> const &value() const;
    /**
     * Indicates whether the SArray should contain integer-values or
     * real values.
     */
    bool isDiscreteValued() const;
    /**
     * Returns the range associated with the SArray.
     */
    Range const &range() const;
    /**
     * Returns a vector of strings giving names corresponding to the
     * dimensions of the SArray. A newly created SArray has no
     * dimnames and this function returns an empty vector.
     */
    std::vector<std::string> const &dimNames() const;
    /**
     * Sets the dimnames of the SArray.
     *
     * @param names A vector of names of size equal to the number of
     * dimensions, or zero.
     */
    void setDimNames(std::vector<std::string> const &names);
    /**
     * It is convenient to inline these functions so that an SArray
     * can be thought of as having some of the dimension attributes of
     * its associated range.
     */
    unsigned int length() const { return range().length(); }
    unsigned int ndim(bool drop) const { return range().ndim(drop); }
    std::vector<unsigned int> const &
	dim(bool drop) const { return range().dim(drop); }
};

#endif /* SARRAY_H_ */

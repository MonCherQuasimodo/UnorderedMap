#ifndef UNORDEREDMAP_H
#define UNORDEREDMAP_H

#include <iterator>
#include <cmath>
#include <stdexcept>
#include <iostream>
#include <list>
#include <type_traits>

template<typename vertex, typename Allocator = std::allocator<vertex>>
class List {
private:
    struct Node {
        vertex a;
        Node* next;
        Node* prev;
        Node(const vertex& value) :
            a(value),
            next(nullptr),
            prev(nullptr) {}
        Node(vertex && value) :
            a(std::move(value)),
            next(nullptr),
            prev(nullptr) {}
    };

    unsigned size_ = 0;
    Node* begin_ = nullptr;
    Node* end_ = nullptr;

    using AllocatorNode = typename std::allocator_traits<Allocator>::template rebind_alloc<Node>;
    using AllocatorTraits = std::allocator_traits<AllocatorNode>;

    Allocator allocT_;
    AllocatorNode allocNode_;

    void copy_(const List& right);
    Node* createOneElement_(const vertex &value);

    void insert_before(Node*, const vertex& value);

    void insert_after(Node*, const vertex& value);
    void insert_after(Node*, vertex&& value);

    void erase_(Node*);
    void insert_after_helper_(Node*, Node*);
    void push_back_helper_(Node*);
    template<bool Const>
    class iterator_;

public:
    explicit List(const Allocator& alloc = Allocator());
    List(size_t count, const vertex& value = vertex(), const Allocator& alloc = Allocator());

    List(const List& right);
    List(List&& right);

    List& operator=(const List& right);
    List& operator=(List&& right);

    ~List();

    unsigned size() const;

    void pop_front();
    void pop_back();

    void push_front(const vertex& value);
    void push_back(const vertex& value);
    void push_back(vertex&& value);

    void clear();
    void clear_ac();
    //___
    using ConstIterator = iterator_<true>;
    using Iterator = iterator_<false>;


    Iterator begin();
    Iterator end();
    ConstIterator cbegin() const;
    ConstIterator cend() const;

    //Вставке перед pos, возвращаем итератор на вставленный элемент
    Iterator insert(ConstIterator pos, const vertex& value);
    Iterator insert(ConstIterator pos, vertex&& value);

    Iterator erase(ConstIterator pos);
};

///__________________Construct_Destruct___________///
template<typename vertex, typename Allocator>
List<vertex, Allocator>::List(const Allocator& alloc):
    begin_(nullptr),
    end_(nullptr),
    allocT_(alloc) {}

template<typename vertex, typename Allocator>
List<vertex, Allocator>& List<vertex, Allocator>::operator=(const List<vertex, Allocator>& right) {
    if (this == &right) {
        return *this;
    }
    while (size_) {
        pop_back();
    }
    copy_(right);
    return *this;
}

template<typename vertex, typename Allocator>
List<vertex, Allocator>& List<vertex, Allocator>::operator=(List<vertex, Allocator>&& right) {
    if (this == &right) {
        return *this;
    }
    begin_ = right.begin_;
    right.begin_ = nullptr;
    end_ = right.end_;
    right.end_ = nullptr;
    size_ = right.size_;
    allocT_ = std::move(right.allocT_);
    allocNode_ = std::move(right.allocNode_);
    return *this;
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::copy_(const List &right) {
    Node* start = right.begin_;
    while (start != nullptr) {
        push_back(start->a);
        start = start->next;
    }
}

template<typename vertex, typename Allocator>
List<vertex, Allocator>::List(const List<vertex, Allocator>& right) {
    copy_(right);
}

template<typename vertex, typename Allocator>
List<vertex, Allocator>::List(List<vertex, Allocator>&& right) :
    begin_(right.begin_),
    end_(right.end_),
    size_(right.size_),
    allocT_(std::move(right.allocT_)),
    allocNode_(std::move(right.allocNode_)) {
    right.begin_ = right.end_ = nullptr;
}

template<typename vertex, typename Allocator>
List<vertex, Allocator>::List(size_t count, const vertex& value, const Allocator& alloc) : List(alloc) {
    for (size_t i = 0; i < count; i++) {
        push_back(value);
    }
}

template<typename vertex, typename Allocator>
List<vertex, Allocator>::~List() {
    clear();
}

///_________________Methods_______________________///
template<typename vertex, typename Allocator>
unsigned List<vertex, Allocator>::size() const {
    return size_;
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::pop_front() {
    erase_(begin_);
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::pop_back() {
    erase_(end_);
}

template<typename vertex, typename Allocator>
typename List<vertex, Allocator>::Node* List<vertex, Allocator>::createOneElement_(const vertex &value) {
    Node* newElem = AllocatorTraits::allocate(allocNode_, size_t(true));
    AllocatorTraits::construct(allocNode_, newElem, value);
    return newElem;
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::push_front(const vertex& value) {
    Node* newElem = createOneElement_(value);
    push_back_helper_(newElem);
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::push_back_helper_(Node* newElem) {
    newElem->next = begin_;
    if (begin_) {
        begin_->prev = newElem;
    } else {
        end_ = newElem;
    }
    begin_ = newElem;
    ++size_;
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::push_back(const vertex &value) {
    Node* newElem = createOneElement_(value);
    push_back_helper_(newElem);
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::push_back(vertex&& value) {
    Node* newElem = AllocatorTraits::allocate(allocNode_, size_t(true));
    AllocatorTraits::construct(allocNode_, newElem, std::move(value));
    push_back_helper_(newElem);
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::insert_before(Node* p, const vertex& value) {
    Node* newElem = createOneElement_(value);
    newElem->next = p;
    newElem->prev = p->prev;
    if (p->prev) {
        p->prev->next = newElem;
    }
    p->prev = newElem;
    if (newElem->prev == nullptr)
        begin_ = newElem;
    ++size_;
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::insert_after_helper_(Node* p, Node* newElem) {
    newElem->prev = p;
    newElem->next = p->next;
    if (p->next) {
        p->next->prev = newElem;
    }
    p->next = newElem;
    if (newElem->next == nullptr)
        end_ = newElem;
    ++size_;
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::insert_after(Node* p, const vertex& value) {
    Node* newElem = createOneElement_(value);
    insert_after_helper_(p, newElem);
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::insert_after(Node* p, vertex&& value) {
    Node* newElem = AllocatorTraits::allocate(allocNode_, size_t(true));
    AllocatorTraits::construct(allocNode_, newElem, std::move(value));
    insert_after_helper_(p, newElem);
}


template<typename vertex, typename Allocator>
void List<vertex, Allocator>::erase_(Node* p) {
    if (p == nullptr || !size_) {
        return;
    }

    if (p->next) {
        p->next->prev = p->prev;
    } else {
        end_ = p->prev;
    }

    if (p->prev) {
        p->prev->next = p->next;
    } else {
        begin_ = p->next;
    }

    AllocatorTraits::destroy(allocNode_, p);
    AllocatorTraits::deallocate(allocNode_, p, size_t(true));
    --size_;
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::clear() {
    while (size_ > 0) {
        pop_back();
    }
}

template<typename vertex, typename Allocator>
void List<vertex, Allocator>::clear_ac() {
    begin_ = end_ = nullptr;
    size_ = 0;
}

template<typename vertex, typename Allocator>
typename List<vertex, Allocator>::Iterator List<vertex, Allocator>::begin() {
    return Iterator(begin_);
}

template<typename vertex, typename Allocator>
typename List<vertex, Allocator>::Iterator  List<vertex, Allocator>::end() {
    return Iterator(nullptr);
}

template<typename vertex, typename Allocator>
typename List<vertex, Allocator>::ConstIterator List<vertex, Allocator>::cbegin() const {
    return ConstIterator(begin_);
}

template<typename vertex, typename Allocator>
typename List<vertex, Allocator>::ConstIterator List<vertex, Allocator>::cend() const {
    return ConstIterator(nullptr);
}

template<typename vertex, typename Allocator>
typename List<vertex, Allocator>::Iterator List<vertex, Allocator>::insert(typename List<vertex, Allocator>::ConstIterator pos, const vertex& value) {

    if (pos == cend()) {
        push_back(value);
        return Iterator(end_);
    } else {
        const Node* ptr = pos.get_iter_();
        insert_after(const_cast<Node*>(ptr), value);
        return Iterator(ptr->next);
    }
}

template<typename vertex, typename Allocator>
typename List<vertex, Allocator>::Iterator List<vertex, Allocator>::insert(typename List<vertex, Allocator>::ConstIterator pos, vertex&& value) {
    if (pos == cend()) {
        push_back(std::move(value));
        return Iterator(end_);
    } else {
        const Node* ptr = pos.get_iter_();
        insert_after(const_cast<Node*>(ptr), std::move(value));
        return Iterator(ptr->next);
    }
}

template<typename vertex, typename Allocator>
typename List<vertex, Allocator>::Iterator List<vertex, Allocator>::erase(typename List<vertex, Allocator>::ConstIterator pos) {
    if (pos == cend()) {
        return end();
    } else {
        const Node* ptr = pos.get_iter_();
        Node* return_ptr = ptr->next;
        erase_(const_cast<Node*>(ptr));
        return Iterator(return_ptr);
    }
}

template<typename vertex, typename Allocator>
template<bool Const>
class List<vertex, Allocator>::iterator_ {
public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = vertex;
    using difference_type = size_t;
    using pointer = typename std::conditional_t<Const, vertex const*, vertex*>;
    using reference = typename std::conditional_t<Const, vertex const&, vertex&>;
private:
    Node* node_iter_;
    iterator_() = delete;
    explicit iterator_(Node* node_iter) :
        node_iter_(node_iter) {};

    template<typename = std::enable_if<!Const>>
    iterator_(const iterator_<true>& other) :
              node_iter_(other.node_iter_) {}

    Node* get_iter_() {
        return node_iter_;
    }
public:
    iterator_(const iterator_<false>& other) :
              node_iter_(other.node_iter_) {}

    iterator_& operator++() {
        node_iter_ = node_iter_->next;
        return *this;
    }
    iterator_ operator++(int) {
        iterator_ other = *this;
        ++(*this);
        return other;
    }
    iterator_& operator--() {
        node_iter_ = node_iter_->prev;
        return *this;
    }
    iterator_ operator--(int) {
        iterator_ other = *this;
        --(*this);
        return other;
    }
    reference operator*() {
        return node_iter_->a;
    }
    pointer operator->() {
        return &(node_iter_->a);
    }
    bool operator==(const iterator_& other) const {
        return node_iter_ == other.node_iter_;
    }
    bool operator!=(const iterator_& other) const {
        return node_iter_ != other.node_iter_;
    }
    friend class List;
    friend class iterator_<false>;
};

template <
    typename Key,
    typename Value,
    typename Hash = std::hash<Key>,
    typename Equal = std::equal_to<Key>,
    typename Alloc = std::allocator<std::pair<const Key, Value>>
> class UnorderedMap {
public:
    using NodeType = std::pair<const Key, Value>;

    template<bool Const>
    class iterator_;
    using ConstIterator = iterator_<true>;
    using Iterator = iterator_<false>;

    UnorderedMap();
    UnorderedMap(const UnorderedMap &);
    UnorderedMap(UnorderedMap &&);

    UnorderedMap& operator=(const UnorderedMap &);
    UnorderedMap& operator=(UnorderedMap &&);

    ~UnorderedMap();

    Value& operator[](Key);
    Value& at(Key);

    Iterator begin();
    Iterator end();
    ConstIterator cbegin();
    ConstIterator cend();

    template<class InputIt>
    void insert(InputIt, InputIt);
    std::pair<Iterator, bool> insert(const NodeType&);

    template<class P>
    std::pair<Iterator, bool> insert(P&&);


    template<class... Args>
    std::pair<Iterator, bool> emplace(Args&&...);


    Iterator erase(ConstIterator);
    Iterator erase(ConstIterator, ConstIterator);

    Iterator find(const Key &);

    size_t size() const;
    void reserve(size_t);
    float load_factor() const;
    void max_load_factor(float);
    float max_load_factor() const;

private:        
    static constexpr float initialize_max_load_factor = 0.75;

    using AllocatorList = typename std::allocator_traits<Alloc>::template rebind_alloc<NodeType*>;

    using listIterator = typename std::list<NodeType*, AllocatorList>::iterator;
    using constListIterator = typename std::list<NodeType*, AllocatorList>::const_iterator;
    //using listIterator = typename List<NodeType*, AllocatorList>::Iterator;
    //using constListIterator = typename List<NodeType*, AllocatorList>::ConstIterator;

    using AllocatorIterator = typename std::allocator_traits<Alloc>::template rebind_alloc<listIterator>;
    using AllocatorTraits = std::allocator_traits<AllocatorIterator>;

    size_t sizeArray_;
    listIterator* array_;

    std::list<NodeType*, AllocatorList> data_;
    //List<NodeType*, AllocatorList> data_;
    float maxLoadFactorValue = initialize_max_load_factor;

    Hash Hash_;
    AllocatorIterator Alloc_;
    Alloc AllocNode_;
    Equal EqualKey_;

private: //support
    std::pair<Iterator, bool> insert_(NodeType *);
    void cut_and_past_(UnorderedMap&& from);
    void fill_default_();
    void checkLoadFactor_();
    void rehash_(size_t);
    size_t currentHash_(const Key &) const;
};

///_________________Constructor_Destructor_Assignment___________________///
template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::UnorderedMap() :
    sizeArray_(1),
    array_(AllocatorTraits::allocate(Alloc_, 1)) {
    fill_default_();
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::cut_and_past_(UnorderedMap&& from) {
    sizeArray_ = from.sizeArray_,
    array_ = from.array_;
    from.array_ = nullptr;
    data_ = std::move(from.data_);
    maxLoadFactorValue = from.maxLoadFactorValue;
    Hash_ = std::move(from.Hash_);
    Alloc_ = std::move(from.Alloc_);
    AllocNode_ = std::move(AllocNode_);
    EqualKey_ = std::move(from.EqualKey_);
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::UnorderedMap(const UnorderedMap & right) :
    sizeArray_(right.sizeArray_),
    array_(AllocatorTraits::allocate(Alloc_, right.sizeArray_)),
    data_(right.data_),
    maxLoadFactorValue(right.maxLoadFactorValue),
    Hash_(right.Hash_),
    Alloc_(right.Alloc_),
    AllocNode_(right.AllocNode_),
    EqualKey_(right.EqualKey_){
    fill_default_();
    for (listIterator it = data_.begin(); it != data_.end(); ++it) {
        size_t hash = currentHash_((*it)->first);
        if (array_[hash] == data_.end())
            array_[hash] = it;
    }
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::UnorderedMap(UnorderedMap && right) {
    cut_and_past_(std::move(right));
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
UnorderedMap<Key, Value, Hash, Equal, Alloc>& UnorderedMap<Key, Value, Hash, Equal, Alloc>::operator=(const UnorderedMap & right) {
    if(this != &right) {
        UnorderedMap temp(right);
        cut_and_past_(std::move(temp));
    }
    return *this;
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
UnorderedMap<Key, Value, Hash, Equal, Alloc>& UnorderedMap<Key, Value, Hash, Equal, Alloc>::operator=(UnorderedMap && right) {
    if(this != &right) {
        UnorderedMap temp(std::move(right));
        cut_and_past_(std::move(temp));
    }
    return *this;
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::~UnorderedMap() {
    AllocatorTraits::deallocate(Alloc_, array_, sizeArray_);
    for (auto i = data_.begin(); i != data_.end(); ++i) {
        delete *i;
    }
}

///____________________Iterators_________________________///
template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
template<bool Const>
class UnorderedMap<Key, Value, Hash, Equal, Alloc>::iterator_ {
public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = NodeType;
    using difference_type = size_t;
    using pointer = typename std::conditional_t<Const, NodeType const*, NodeType*>;
    using reference = typename std::conditional_t<Const, NodeType const&, NodeType&>;

private:
    listIterator node_iter_;
    explicit iterator_(listIterator node_iter) :
        node_iter_(node_iter) {};

    template<typename = std::enable_if<!Const>>
    iterator_(const iterator_<true>& other) :
              node_iter_(other.node_iter_) {}

    listIterator get_iter_() {
        return node_iter_;
    }
public:
    iterator_(const iterator_<false>& other) :
              node_iter_(other.node_iter_) {}

    iterator_& operator++() {
        ++node_iter_;
        return *this;
    }
    iterator_ operator++(int) {
        iterator_ other = *this;
        ++(*this);
        return other;
    }
    reference operator*() {
        return *(*node_iter_);
    }
    pointer operator->() {
        return *node_iter_;
    }
    bool operator==(const iterator_& other) const {
        return listIterator(node_iter_) == listIterator(other.node_iter_);
    }
    bool operator!=(const iterator_& other) const {
        return node_iter_ != other.node_iter_;
    }
    friend class UnorderedMap;
    friend class iterator_<false>;
};

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator
         UnorderedMap<Key, Value, Hash, Equal, Alloc>::begin() {
    return Iterator(data_.begin());
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator
           UnorderedMap<Key, Value, Hash, Equal, Alloc>::end() {
    return Iterator(data_.end());
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::ConstIterator
           UnorderedMap<Key, Value, Hash, Equal, Alloc>::cbegin() {
    return ConstIterator(data_.begin());
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::ConstIterator
           UnorderedMap<Key, Value, Hash, Equal, Alloc>::cend() {
    return ConstIterator(data_.end());
}

///__________________________Size_Handlers_____________________________///
template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
size_t UnorderedMap<Key, Value, Hash, Equal, Alloc>::size() const {
    return data_.size();
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::reserve(size_t count) {
    if (count > data_.size())
        rehash_(std::ceil(count / max_load_factor()));
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
float UnorderedMap<Key, Value, Hash, Equal, Alloc>::load_factor() const {
    return static_cast<float>(data_.size()) / sizeArray_;
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::max_load_factor(float newValue) {
    maxLoadFactorValue = newValue;
    checkLoadFactor_();
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
float UnorderedMap<Key, Value, Hash, Equal, Alloc>::max_load_factor() const {
    return maxLoadFactorValue;
}

///_________________________________Element_Handlers_____________________________///

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
Value& UnorderedMap<Key, Value, Hash, Equal, Alloc>::operator[](Key key) {
    try {
        return at(key);
    }  catch (std::out_of_range& error) {
        auto pair = insert({key, Value()});
        return pair.first->second;
    }
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
Value& UnorderedMap<Key, Value, Hash, Equal, Alloc>::at(Key key) {
    size_t hash = currentHash_(key);
    listIterator it = array_[hash];
    while (it != data_.end() && currentHash_((*it)->first) == hash) {
        if (EqualKey_((*it)->first, key)) {
            return (*it)->second;
        }
        ++it;
    }
    throw std::out_of_range("Unexisting element");
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator
         UnorderedMap<Key, Value, Hash, Equal, Alloc>::find(const Key& key) {
    size_t hash = currentHash_(key);
    listIterator it = array_[hash];
    while (it != data_.end() && currentHash_((*it)->first) == hash) {
        if (EqualKey_((*it)->first, key)) {
            return Iterator(it);
        }
        ++it;
    }
    return Iterator(data_.end());
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
std::pair<typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator, bool> UnorderedMap<Key, Value, Hash, Equal, Alloc>::insert_(NodeType* pair) {
    if (pair == nullptr)
        return {end(), false};
    if (find(pair->first) != end()) {
        return {find(pair->first), false};
    }
    checkLoadFactor_();
    size_t hash = currentHash_(pair->first);
    array_[hash] = data_.insert(array_[hash], std::move(pair));
    return {Iterator(array_[hash]), true};
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
std::pair<typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator, bool> UnorderedMap<Key, Value, Hash, Equal, Alloc>::insert(const NodeType& pair) {
    NodeType* ptr = std::allocator_traits<Alloc>::allocate(AllocNode_, 1);
    std::allocator_traits<Alloc>::construct(AllocNode_, ptr, pair);
    return insert_(ptr);
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
template <typename P>
std::pair<typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator, bool> UnorderedMap<Key, Value, Hash, Equal, Alloc>::insert(P&& pair) {
    return emplace(std::forward<P>(pair));
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
template<class... Args>
std::pair<typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator, bool> UnorderedMap<Key, Value, Hash, Equal, Alloc>::emplace(Args&&... args) {
    NodeType* ptr = std::allocator_traits<Alloc>::allocate(AllocNode_, 1);
    std::allocator_traits<Alloc>::construct(AllocNode_, ptr, std::forward<Args>(args)...);
    return insert_(ptr);
}


template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
template <typename InputIt>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::insert(InputIt first, InputIt last) {
    for (; first != last; insert(*first++));
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator UnorderedMap<Key, Value, Hash, Equal, Alloc>::erase(ConstIterator pos) {
    if (pos == cend())
        return end();
    size_t hash = currentHash_(pos->first);
    if (pos.get_iter_() == array_[hash]) {
        ConstIterator it = pos;
        ++it;
        if (it != cend() && currentHash_(it->first) != hash) {
            array_[hash] = data_.end();
            return Iterator(data_.erase(pos.get_iter_()));
        } else {
            return Iterator(array_[hash] = data_.erase(pos.get_iter_()));
        }
    }
    return Iterator(data_.erase(pos.get_iter_()));
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator UnorderedMap<Key, Value, Hash, Equal, Alloc>::erase(ConstIterator first, ConstIterator last) {
    for (ConstIterator it = first; it != last; it = erase(it));
    return end(); //Грубо.
}

///______________________________Support__________________________________///

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::fill_default_() {
    std::fill(array_, array_ + sizeArray_, data_.end());
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
size_t UnorderedMap<Key, Value, Hash, Equal, Alloc>::currentHash_(const Key& key) const {
    return Hash_(key) % sizeArray_;
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::rehash_(size_t count) {
    AllocatorTraits::deallocate(Alloc_, array_, sizeArray_);
    sizeArray_ = count;
    array_ = AllocatorTraits::allocate(Alloc_, sizeArray_);
    fill_default_();

    std::list<NodeType*, AllocatorList> temp = std::move(data_);
    data_.clear();
    for (auto i = temp.begin(); i != temp.end(); ++i) {
        size_t hash = currentHash_((*i)->first);
        if (array_[hash] == data_.end()) {
            array_[hash] = data_.insert(data_.end(), *i);
        } else {
            data_.insert(array_[hash], *i);
            --array_[hash];
        }
    }
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::checkLoadFactor_() {
    if (load_factor() > maxLoadFactorValue)
        reserve(sizeArray_ * 2);
    // Можно еще и уменьшаться, load_factor очень мал.
}
#endif // UNORDEREDMAP_H

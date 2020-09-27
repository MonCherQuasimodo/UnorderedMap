#ifndef UNORDEREDMAP_H
#define UNORDEREDMAP_H

#include <iterator>
#include <cmath>
#include <stdexcept>
#include <iostream>
#include <type_traits>

template<typename T, typename Allocator = std::allocator<T>>
class List {
private:
    struct Node {
        T a;
        Node* next;
        Node* prev;
        Node(const T& value) :
            a(value),
            next(nullptr),
            prev(nullptr) {}
        Node(T && value) :
            a(std::move(value)),
            next(nullptr),
            prev(nullptr) {}
    };

    unsigned size_;
    Node* begin_;
    Node* end_;

    using AllocatorNode = typename std::allocator_traits<Allocator>::template rebind_alloc<Node>;
    using AllocatorTraits = std::allocator_traits<AllocatorNode>;

    Allocator allocT_;
    AllocatorNode allocator_node_;

    void copy_(const List& right);
    Node* createOneElement_(const T &value);

    void insert_before(Node*, const T& value);
    void insert_before(Node*, T&& value);
    void insert_before_helper_(Node*, Node*);

    void insert_after(Node*, const T& value);
    void insert_after(Node*, T&& value);
    void insert_after_helper_(Node*, Node*);

    void erase_(Node*);
    void push_back_helper_(Node*);

    template<bool Const>
    class iterator_;

public:
    explicit List(const Allocator& alloc = Allocator());
    List(size_t count, const T& value = T(), const Allocator& alloc = Allocator());

    List(const List& right);
    List(List&& right);

    List& operator=(const List& right);
    List& operator=(List&& right);

    ~List();

    unsigned size() const;

    void pop_front();
    void pop_back();

    void push_front(const T& value);
    void push_back(const T& value);
    void push_back(T&& value);

    void clear();

    using ConstIterator = iterator_<true>;
    using Iterator = iterator_<false>;

    Iterator begin();
    Iterator end();
    ConstIterator cbegin() const;
    ConstIterator cend() const;

    //Вставке перед pos, возвращаем итератор на вставленный элемент
    Iterator insert(ConstIterator pos, const T& value);
    Iterator insert(ConstIterator pos, T&& value);

    Iterator erase(ConstIterator pos);
};

///__________________Construct_Destruct___________///
template<typename T, typename Allocator>
List<T, Allocator>::List(const Allocator& alloc):
    size_(0),
    begin_(nullptr),
    end_(nullptr),
    allocT_(alloc) {}

template<typename T, typename Allocator>
List<T, Allocator>& List<T, Allocator>::operator=(const List<T, Allocator>& right) {
    if (this == &right) {
        return *this;
    }
    while (size_) {
        pop_back();
    }
    copy_(right);
    return *this;
}

template<typename T, typename Allocator>
List<T, Allocator>& List<T, Allocator>::operator=(List<T, Allocator>&& right) {
    if (this == &right) {
        return *this;
    }
    begin_ = right.begin_;
    right.begin_ = nullptr;
    end_ = right.end_;
    right.end_ = nullptr;
    right.size_ = 0;
    size_ = right.size_;
    allocT_ = std::move(right.allocT_);
    allocator_node_ = std::move(right.allocator_node_);
    return *this;
}

template<typename T, typename Allocator>
void List<T, Allocator>::copy_(const List &right) {
    Node* start = right.begin_;
    while (start != nullptr) {
        push_back(start->a);
        start = start->next;
    }
}

template<typename T, typename Allocator>
List<T, Allocator>::List(const List<T, Allocator>& right) : List(Allocator()){
    copy_(right);
}

template<typename T, typename Allocator>
List<T, Allocator>::List(List<T, Allocator>&& right) :
    size_(right.size_),
    begin_(right.begin_),
    end_(right.end_),
    allocT_(std::move(right.allocT_)),
    allocator_node_(std::move(right.allocator_node_)) {
    right.begin_ = right.end_ = nullptr;
    right.size_ = 0;
}

template<typename T, typename Allocator>
List<T, Allocator>::List(size_t count, const T& value, const Allocator& alloc) : List(alloc) {
    for (size_t i = 0; i < count; i++) {
        push_back(value);
    }
}

template<typename T, typename Allocator>
List<T, Allocator>::~List() {
    clear();
}

///_________________Methods_______________________///
template<typename T, typename Allocator>
unsigned List<T, Allocator>::size() const {
    return size_;
}

template<typename T, typename Allocator>
void List<T, Allocator>::pop_front() {
    erase_(begin_);
}

template<typename T, typename Allocator>
void List<T, Allocator>::pop_back() {
    erase_(end_);
}

template<typename T, typename Allocator>
typename List<T, Allocator>::Node* List<T, Allocator>::createOneElement_(const T &value) {
    Node* newElem = AllocatorTraits::allocate(allocator_node_, size_t(true));
    AllocatorTraits::construct(allocator_node_, newElem, value);
    return newElem;
}

template<typename T, typename Allocator>
void List<T, Allocator>::push_front(const T& value) {
    Node* newElem = createOneElement_(value);
    newElem->next = begin_;
    if (begin_) {
        begin_->prev = newElem;
    } else {
        end_ = newElem;
    }
    begin_ = newElem;
    ++size_;
}

template<typename T, typename Allocator>
void List<T, Allocator>::push_back_helper_(Node* newElem) {
    newElem->prev = end_;
    if (end_) {
        end_->next = newElem;
    } else {
        begin_ = newElem;
    }
    end_ = newElem;
    ++size_;
}

template<typename T, typename Allocator>
void List<T, Allocator>::push_back(const T &value) {
    Node* newElem = createOneElement_(value);
    push_back_helper_(newElem);
}

template<typename T, typename Allocator>
void List<T, Allocator>::push_back(T&& value) {
    Node* newElem = AllocatorTraits::allocate(allocator_node_, size_t(true));
    AllocatorTraits::construct(allocator_node_, newElem, std::move(value));
    push_back_helper_(newElem);
}

template<typename T, typename Allocator>
void List<T, Allocator>::insert_before_helper_(Node* p, Node* newElem) {
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

template<typename T, typename Allocator>
void List<T, Allocator>::insert_after_helper_(Node* p, Node* newElem) {
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

template<typename T, typename Allocator>
void List<T, Allocator>::insert_after(Node* p, const T& value) {
    Node* newElem = createOneElement_(value);
    insert_after_helper_(p, newElem);
}

template<typename T, typename Allocator>
void List<T, Allocator>::insert_after(Node* p, T&& value) {
    Node* newElem = AllocatorTraits::allocate(allocator_node_, size_t(true));
    AllocatorTraits::construct(allocator_node_, newElem, std::move(value));
    insert_after_helper_(p, newElem);
}

template<typename T, typename Allocator>
void List<T, Allocator>::insert_before(Node* p, const T& value) {
    Node* newElem = createOneElement_(value);
    insert_before_helper_(p, newElem);
}

template<typename T, typename Allocator>
void List<T, Allocator>::insert_before(Node* p, T&& value) {
    Node* newElem = AllocatorTraits::allocate(allocator_node_, size_t(true));
    AllocatorTraits::construct(allocator_node_, newElem, std::move(value));
    insert_before_helper_(p, newElem);
}

template<typename T, typename Allocator>
void List<T, Allocator>::erase_(Node* p) {
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

    AllocatorTraits::destroy(allocator_node_, p);
    AllocatorTraits::deallocate(allocator_node_, p, size_t(true));
    --size_;
}

template<typename T, typename Allocator>
void List<T, Allocator>::clear() {
    while (size_ > 0) {
        pop_back();
    }
}

template<typename T, typename Allocator>
typename List<T, Allocator>::Iterator List<T, Allocator>::begin() {
    return Iterator(begin_);
}

template<typename T, typename Allocator>
typename List<T, Allocator>::Iterator  List<T, Allocator>::end() {
    return Iterator(nullptr);
}

template<typename T, typename Allocator>
typename List<T, Allocator>::ConstIterator List<T, Allocator>::cbegin() const {
    return ConstIterator(begin_);
}

template<typename T, typename Allocator>
typename List<T, Allocator>::ConstIterator List<T, Allocator>::cend() const {
    return ConstIterator(nullptr);
}

template<typename T, typename Allocator>
typename List<T, Allocator>::Iterator List<T, Allocator>::insert(typename List<T, Allocator>::ConstIterator pos, const T& value) {
    if (pos == cend()) {
        push_back(value);
        return Iterator(end_);
    } else {
        const Node* ptr = pos.get_iter_();
        insert_before(const_cast<Node*>(ptr), value);
        return Iterator(ptr->prev);
    }
}

template<typename T, typename Allocator>
typename List<T, Allocator>::Iterator List<T, Allocator>::insert(typename List<T, Allocator>::ConstIterator pos, T&& value) {
    if (pos == cend()) {
        push_back(std::move(value));
        return Iterator(end_);
    } else {
        const Node* ptr = pos.get_iter_();
        insert_before(const_cast<Node*>(ptr), std::move(value));
        return Iterator(ptr->prev);
    }
}

template<typename T, typename Allocator>
typename List<T, Allocator>::Iterator List<T, Allocator>::erase(typename List<T, Allocator>::ConstIterator pos) {
    if (pos == cend()) {
        return end();
    } else {
        const Node* ptr = pos.get_iter_();
        Node* return_ptr = ptr->next;
        erase_(const_cast<Node*>(ptr));
        return Iterator(return_ptr);
    }
}

template<typename T, typename Allocator>
template<bool Const>
class List<T, Allocator>::iterator_ {
public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = T;
    using difference_type = size_t;
    using pointer = std::conditional_t<Const, T const*, T*>;
    using reference = std::conditional_t<Const, T const&, T&>;
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

    iterator_& operator=(const iterator_& other) {
        if (this == &other) {
            return *this;
        }
        node_iter_ = other.node_iter_;
        return *this;
    }
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
private:
    template<bool Const>
    class iterator_;
public:
    using NodeType = std::pair<const Key, Value>;

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
    template<bool Const>
    class iterator_;

    static constexpr float initialize_max_load_factor = 0.75;

    using AllocatorList = typename std::allocator_traits<Alloc>::template rebind_alloc<NodeType*>;

    using listIterator = typename List<NodeType*, AllocatorList>::Iterator;
    using constListIterator = typename List<NodeType*, AllocatorList>::ConstIterator;

    using AllocatorIterator = typename std::allocator_traits<Alloc>::template rebind_alloc<listIterator>;
    using AllocatorTraits = std::allocator_traits<AllocatorIterator>;

    size_t size_array_;
    listIterator* array_;

    List<NodeType*, AllocatorList> data_;
    float max_load_factor_value = initialize_max_load_factor;

    Hash hash_func_;
    AllocatorIterator allocator_iter_;
    Alloc allocator_node_;
    Equal equal_key_func_;

private: //support
    std::pair<Iterator, bool> insert_(NodeType *);
    void cut_and_past_(UnorderedMap&& from);
    void fill_default_();
    void check_load_factor_();
    void rehash_(size_t);
    size_t current_hash_func_(const Key &) const;
};

///_________________Constructor_Destructor_Assignment___________________///
template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::UnorderedMap() :
    size_array_(1),
    array_(AllocatorTraits::allocate(allocator_iter_, 1)) {
    fill_default_();
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::cut_and_past_(UnorderedMap&& from) {
    size_array_ = from.size_array_,
    array_ = from.array_;
    from.array_ = nullptr;
    data_ = std::move(from.data_);
    max_load_factor_value = from.max_load_factor_value;
    hash_func_ = std::move(from.hash_func_);
    allocator_iter_ = std::move(from.allocator_iter_);
    allocator_node_ = std::move(allocator_node_);
    equal_key_func_ = std::move(from.equal_key_func_);
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::UnorderedMap(const UnorderedMap & right) :
    size_array_(right.size_array_),
    array_(AllocatorTraits::allocate(allocator_iter_, right.size_array_)),
    data_(right.data_),
    max_load_factor_value(right.max_load_factor_value),
    hash_func_(right.hash_func_),
    allocator_iter_(right.allocator_iter_),
    allocator_node_(right.allocator_node_),
    equal_key_func_(right.equal_key_func_){
    fill_default_();
    for (listIterator it = data_.begin(); it != data_.end(); ++it) {
        size_t hash = current_hash_func_((*it)->first);
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
    AllocatorTraits::deallocate(allocator_iter_, array_, size_array_);
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
        return node_iter_ == other.node_iter_;
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
    return static_cast<float>(data_.size()) / size_array_;
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::max_load_factor(float newValue) {
    max_load_factor_value = newValue;
    check_load_factor_();
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
float UnorderedMap<Key, Value, Hash, Equal, Alloc>::max_load_factor() const {
    return max_load_factor_value;
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
    Iterator it = find(key);
    if (it != end()) {
        return it->second;
    }
    throw std::out_of_range("Unexisting element");
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator
         UnorderedMap<Key, Value, Hash, Equal, Alloc>::find(const Key& key) {
    size_t hash = current_hash_func_(key);
    listIterator it = array_[hash];
    while (it != data_.end() && current_hash_func_((*it)->first) == hash) {
        if (equal_key_func_((*it)->first, key)) {
            return Iterator(it);
        }
        ++it;
    }
    return Iterator(data_.end());
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
std::pair<typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator, bool>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::insert_(NodeType* pair) {
    if (pair == nullptr)
        return {end(), false};
    if (find(pair->first) != end()) {
        return {find(pair->first), false};
    }
    check_load_factor_();
    size_t hash = current_hash_func_(pair->first);
    array_[hash] = data_.insert(array_[hash], std::move(pair));
    return {Iterator(array_[hash]), true};
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
std::pair<typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator, bool>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::insert(const NodeType& pair) {
    NodeType* ptr = std::allocator_traits<Alloc>::allocate(allocator_node_, 1);
    std::allocator_traits<Alloc>::construct(allocator_node_, ptr, pair);
    return insert_(ptr);
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
template <typename P>
std::pair<typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator, bool>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::insert(P&& pair) {
    return emplace(std::forward<P>(pair));
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
template<class... Args>
std::pair<typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator, bool>
UnorderedMap<Key, Value, Hash, Equal, Alloc>::emplace(Args&&... args) {
    NodeType* ptr = std::allocator_traits<Alloc>::allocate(allocator_node_, 1);
    std::allocator_traits<Alloc>::construct(allocator_node_, ptr, std::forward<Args>(args)...);
    return insert_(ptr);
}


template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
template <typename InputIt>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::insert(InputIt first, InputIt last) {
    for (; first != last; insert(*first++));
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
typename UnorderedMap<Key, Value, Hash, Equal, Alloc>::Iterator
UnorderedMap<Key, Value, Hash, Equal, Alloc>::erase(ConstIterator pos) {
    if (pos == cend())
        return end();
    size_t hash = current_hash_func_(pos->first);
    if (pos.get_iter_() == array_[hash]) {
        ConstIterator it = pos;
        ++it;
        if (it != cend() && current_hash_func_(it->first) != hash) {
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
    return last;
}

///______________________________Support__________________________________///

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::fill_default_() {
    std::fill(array_, array_ + size_array_, data_.end());
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
size_t UnorderedMap<Key, Value, Hash, Equal, Alloc>::current_hash_func_(const Key& key) const {
    return hash_func_(key) % size_array_;
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::rehash_(size_t count) {
    AllocatorTraits::deallocate(allocator_iter_, array_, size_array_);
    size_array_ = count;
    array_ = AllocatorTraits::allocate(allocator_iter_, size_array_);
    fill_default_();
    List<NodeType*, AllocatorList> temp = std::move(data_);
    for (auto i = temp.begin(); i != temp.end(); ++i) {
        size_t hash = current_hash_func_((*i)->first);
        if (array_[hash] == data_.end()) {
            array_[hash] = data_.insert(data_.end(), *i);
        } else {
            data_.insert(array_[hash], *i);
            --array_[hash];
        }
    }
}

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc>
void UnorderedMap<Key, Value, Hash, Equal, Alloc>::check_load_factor_() {
    if (load_factor() > max_load_factor_value)
        reserve(size_array_ * 2);
    // Можно еще и уменьшаться, load_factor очень мал.
}
#endif // UNORDEREDMAP_H

try {

} catch (ba) {
    var ba; // Should be merged with `catch (ba)`.
    var ca; // Should be moved to global scope.
}

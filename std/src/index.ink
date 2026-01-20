trait indexable
	fn index<I: int>(self, idx: I) -> i64
	fn index_set<I: int>(self, idx: I, value: i64)

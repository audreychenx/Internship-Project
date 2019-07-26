class MyHashMap:

    def __init__(self):
        self.keys = []
        self.values = []
        

    def put(self, key: int, value: int):
        if value >= 0:
            if key in self.keys:
                loc = self.keys.index(key)
                self.values[loc] = value
            else:
                self.keys.append(key)
                self.values.append(value)
        

    def get(self, key: int):
        if key in self.keys:
            loc = self.keys.index(key)
            return self.values[loc]
        else:
            return -1
        

    def remove(self, key: int):
        if key in self.keys:
            index = self.keys.index(key)
            self.keys.pop(index)
            self.values.pop(index)


class GameError(Exception):
    pass

class TextError(GameError):
    def __init__(self, text):
        super.__init__()
        self.text = text

class ChanceBoxError(TextError):
    def __init__(self, text):
        super.__init__(text=text)

class UnreachableError(TextError):
    def __init__(self, text):
        super.__init__(text=text)
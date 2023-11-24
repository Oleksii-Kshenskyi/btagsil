from game.error import ChanceBoxError

class ChanceBox:
    def __init__(self, box_list: list[str]):
        if type(box_list) is not list:
            raise ChanceBoxError(text="ChanceBox needs a list as the initializer.")
        self.box = box_list
    def to_lines(self) -> str:
        return "\n".join(self.box)
    def to_words(self) -> str:
        return " ".join(self.box)
    def to_id(self) -> str:
        return '-'.join(self.box).lower()

def box(box_list: list[str]) -> ChanceBox:
    return ChanceBox(box_list)
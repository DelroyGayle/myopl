import basic

while True:
    text = input('basic > ')
    if text.strip() == "":
        continue
    result, error = basic.run('<stdin>', text)

    if error:
        print(error.as_string())
    elif result:
        # print a single element
        if len(result.elements) == 1:
            print(repr(result.elements[0]))
        else:
        # otherwise print as a list
            print(repr(result))

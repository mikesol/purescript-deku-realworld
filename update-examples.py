import os

examples = os.listdir('gh-examples')
wiki = os.listdir('wiki')
processed = []

for x in wiki:
    with open(f'wiki/{x}', 'r') as infi:
        spans = []
        lines = infi.read().split('\n')
        for ix, line in enumerate(lines):
            if '[Try Me]' in line:
                toUse = line.split('(')[1][:-1].split('/')[-1]
                with open(f'gh-examples/{toUse}', 'r') as inpurs:
                    spans = [{"newText":inpurs.read()}] + spans
            if '```purescript' in line:
                spans[0]["start"] = ix
            elif '```' in line:
                spans[0]["end"] = ix
        for span in spans:
            if "start" not in span or "end" not in span:
                raise ValueError('Invalid span')
        for span in spans:
            lines = lines[:span["start"]+1] + [span["newText"]] + lines[span["end"]:]
        with open(f'wiki/{x}', 'w') as oufi:
            oufi.write('\n'.join(lines))
            

"""
Merge all thesis chapters into one document.
Adds title page, fixes Ch3 sentence, inserts page breaks between chapters.
"""
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.style import WD_STYLE_TYPE
from docx.oxml.ns import qn
from docx.oxml import OxmlElement
import copy
import os

THESIS_DIR = '/sessions/charming-modest-galileo/mnt/Thesis/'
OUT_PATH   = '/sessions/charming-modest-galileo/mnt/Thesis/thesis_complete.docx'

# ── Chapter order ─────────────────────────────────────────────────────────────
CHAPTERS = [
    # (label, [files...])  — multiple files = merge sections in sequence
    ('Abstract',          ['abstract_v2.docx']),
    ('Introduction',      ['chapter1_introduction_v2.docx']),
    ('Literature Review', ['chapter2_literature_v2.docx', 'chapter2_expand.docx']),
    ('Methodology',       ['chapter3_methods_v2.docx', 'chapter3_stationarity.docx', 'chapter3_data_pipeline.docx']),
    ('Rule Extraction',   ['chapter4_v4.docx', 'chapter4_overlap_freq.docx']),
    ('Prediction',        ['chapter5_prediction_v2.docx', 'chapter5_per_scenario.docx', 'chapter5_expand.docx']),
    ('Discussion',        ['chapter6_discussion.docx', 'chapter6_expand.docx']),
    ('Conclusions',       ['chapter7_v3.docx']),
    ('Figures',           ['figures.docx']),
    ('References',        ['references_v2.docx']),
    ('Appendix A',        ['appendix_a_rules.docx']),
    ('Appendix B',        ['appendix_b_eda.docx']),
    ('Appendix C',        ['appendix_c_features.docx']),
]

# ── Helpers ───────────────────────────────────────────────────────────────────
def add_page_break(doc):
    p = doc.add_paragraph()
    run = p.add_run()
    run.add_break(break_type=None)
    p.clear()
    p = doc.add_paragraph()
    pPr = p._p.get_or_add_pPr()
    pgBr = OxmlElement('w:pageBreakBefore')
    pgBr.set(qn('w:val'), '1')
    pPr.append(pgBr)
    return p

def copy_element(elem):
    return copy.deepcopy(elem)

def append_doc(master, source_path):
    """Append all body elements from source_path into master doc."""
    if not os.path.exists(source_path):
        print(f'  MISSING: {source_path}')
        return
    src = Document(source_path)
    for elem in src.element.body:
        tag = elem.tag.split('}')[-1] if '}' in elem.tag else elem.tag
        if tag == 'sectPr':
            continue  # skip section properties — use master's
        master.element.body.append(copy_element(elem))

def add_title_page(doc):
    """Add a simple title page."""
    # University
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('PORTO UNIVERSITY')
    run.font.size = Pt(14)
    run.font.bold = True

    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('Faculty of Engineering of the University of Porto')
    run.font.size = Pt(12)

    # Spacing
    for _ in range(4):
        doc.add_paragraph()

    # Title
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('Sub-Group Discovery and Meaningful Distribution Rules\nin Time Series for Wine Production Forecasting')
    run.font.size = Pt(18)
    run.font.bold = True

    doc.add_paragraph()
    doc.add_paragraph()

    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('MSc Thesis in Data Science and Engineering')
    run.font.size = Pt(13)
    run.font.italic = True

    for _ in range(5):
        doc.add_paragraph()

    # Author
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('Hugo Nogueira')
    run.font.size = Pt(13)
    run.font.bold = True

    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('up202103617')
    run.font.size = Pt(12)

    for _ in range(3):
        doc.add_paragraph()

    # Supervisors
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('Supervised by:')
    run.font.size = Pt(12)

    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('Prof. João Pedro Mendes Moreira (FEUP)')
    run.font.size = Pt(12)

    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('Prof. Mário Campos Cunha (FCUP)')
    run.font.size = Pt(12)

    for _ in range(3):
        doc.add_paragraph()

    # Date
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run('June 2026')
    run.font.size = Pt(12)

# ── Ch3 scenario sentence injection ──────────────────────────────────────────
CH3_INJECT_AFTER = 'This generates 18 evaluation scenarios for RDD'
CH3_SENTENCE = (
    'Specifically, with a minimum training fraction of 80% applied to 89 RDD observations, '
    'the minimum training window covers 71 years (1934–2004), leaving 18 subsequent years '
    '(2005–2022) as sequential test years — one per scenario. For RVV, 80% of 80 observations '
    'yields a minimum training window of 64 years (1942–2005), leaving 16 test years (2006–2021).'
)

def inject_ch3_sentence(doc):
    """Find the walk-forward section and inject the scenario calculation sentence."""
    for i, para in enumerate(doc.paragraphs):
        if CH3_INJECT_AFTER in para.text:
            # Insert a new paragraph after this one
            new_p = OxmlElement('w:p')
            # Copy run formatting from this paragraph
            new_r = OxmlElement('w:r')
            new_rPr = OxmlElement('w:rPr')
            rFonts = OxmlElement('w:rFonts')
            rFonts.set(qn('w:ascii'), 'Times New Roman')
            rFonts.set(qn('w:hAnsi'), 'Times New Roman')
            new_rPr.append(rFonts)
            sz = OxmlElement('w:sz'); sz.set(qn('w:val'), '24')
            new_rPr.append(sz)
            new_r.append(new_rPr)
            t = OxmlElement('w:t')
            t.text = CH3_SENTENCE
            new_r.append(t)
            new_p.append(new_r)
            # Insert after current paragraph
            para._p.addnext(new_p)
            print(f'  ✓ Ch3 sentence injected after paragraph {i}')
            return True
    print('  ⚠ Ch3 inject target not found — sentence not added')
    return False

# ── Build master document ─────────────────────────────────────────────────────
print('Building thesis_complete.docx ...')

master = Document()
# Set default font
style = master.styles['Normal']
style.font.name = 'Times New Roman'
style.font.size = Pt(12)

# Set page margins (A4, 2.5cm left, 2cm others)
section = master.sections[0]
section.page_width  = Cm(21)
section.page_height = Cm(29.7)
section.left_margin   = Cm(2.5)
section.right_margin  = Cm(2.0)
section.top_margin    = Cm(2.5)
section.bottom_margin = Cm(2.0)

# Title page
print('Adding title page...')
add_title_page(master)

# Append all chapters with page breaks between them
for label, files in CHAPTERS:
    print(f'\nChapter: {label}')
    # Page break before each chapter (except first after title page)
    p = master.add_paragraph()
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    pPr = p._p.get_or_add_pPr()
    pgBr = OxmlElement('w:pageBreakBefore')
    pgBr.set(qn('w:val'), '1')
    pPr.append(pgBr)

    for fname in files:
        fpath = os.path.join(THESIS_DIR, fname)
        print(f'  Appending {fname}...')

        # Special case: inject Ch3 sentence into methods
        if fname == 'chapter3_methods_v2.docx':
            tmp_doc = Document(fpath)
            inject_ch3_sentence(tmp_doc)
            tmp_path = '/tmp/ch3_patched.docx'
            tmp_doc.save(tmp_path)
            append_doc(master, tmp_path)
        else:
            append_doc(master, fpath)

# Save
print(f'\nSaving to {OUT_PATH}...')
master.save(OUT_PATH)
print(f'Done. File size: {os.path.getsize(OUT_PATH)//1024} KB')

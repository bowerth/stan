##############################################
# http://flask.pocoo.org/docs/api/#flask.Flask
##############################################

###################################################################################################
# http://stackoverflow.com/questions/18263671/using-markdown-formatting-on-queried-sql-in-flask-app
###################################################################################################
# import markdown
# from flask import Flask
# from flask import render_template
# from flask import Markup

# app = Flask(__name__)
# @app.route('/')

# def index():
#     content = """
#     Chapter
#     =======
    
#     Section
#     -------
    
#     * Item 1
#     * Item 2
#     """
#     content = Markup(markdown.markdown(content))
#     return render_template('index.html', **locals())
    
# app.run(debug=True)

######################################################################
# http://randykarels.com/blog/frictionless-web-development-with-flask/
######################################################################

# import markdown
# foo = "# hello there, **Mr Bond**."
# foo_html = markdown.markdown(foo)
# print(foo_html)     
# # <h1>hello there, <strong>Mr Bond</strong>.</h1>

# import markdown
# md = markdown.Markdown(extensions=['codehilite'])
# foo = """
# Beware the dreaded __trailing comma__.
# Internet explorer will choke.

#     :::javascript
#     var foo = ['andrew', 'birds', 'bowl of', 'fire',];
#     console.log(foo);
# """
# foo_html = md.convert(foo)
# print(foo_html)

from flask import Flask, render_template
# from flaskext.markdown import Markdown
import yaml

app = Flask(__name__)
md = Markdown(app, extensions = ['codehilite'] )

@app.route('/')
def index():
    post = yaml.load(file('content/mypost.yaml', 'r'))
    return render_template('mytemplate.html', post=post )

if __name__ == '__main__':
    app.run()

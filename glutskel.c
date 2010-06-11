/**
 * A skeleton/template GLUT program
 *
 * Written by Brian Paul and in the public domain.
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <GL/glut.h>
#include "glh/glh_glut.h"

static int Win;
static int WinWidth = 400, WinHeight = 400;
static GLfloat Xrot = 0, Yrot = 0, Zrot = 0;
static GLboolean Anim = GL_FALSE;
glh::glut_simple_mouse_interactor object;
glh::glut_callbacks cb;

static void
Idle(void)
{
  //Xrot += 3.0;
  //Yrot += 4.0;
  //Zrot += 2.0;
  object.trackball.increment_rotation();
  glutPostRedisplay();
}


static void
Draw(void)
{
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   //
   glPushMatrix();
   //glRotatef(Xrot, 1, 0, 0);
   //glRotatef(Yrot, 0, 1, 0);
   //glRotatef(Zrot, 0, 0, 1);
   object.apply_transform();
   glutSolidCube(2.0);

   glPopMatrix();

   glutSwapBuffers();
}


static void
Reshape(int width, int height)
{
   WinWidth = width;
   WinHeight = height;
   glViewport(0, 0, width, height);
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glFrustum(-1.0, 1.0, -1.0, 1.0, 5.0, 25.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   //glTranslatef(0.0, 0.0, -15.0);
}


static void
Key(unsigned char key, int x, int y)
{
   const GLfloat step = 3.0;
   (void) x;
   (void) y;
   switch (key) {
   case 'a':
      Anim = !Anim;
      if (Anim)
         glutIdleFunc(Idle);
      else
         glutIdleFunc(NULL);
      break;
   case 'z':
      Zrot -= step;
      break;
   case 'Z':
      Zrot += step;
      break;
   case 27:
      glutDestroyWindow(Win);
      exit(0);
      break;
   }
   glutPostRedisplay();
}


static void
SpecialKey(int key, int x, int y)
{
   const GLfloat step = 3.0;
   (void) x;
   (void) y;
   switch (key) {
   case GLUT_KEY_UP:
      Xrot -= step;
      break;
   case GLUT_KEY_DOWN:
      Xrot += step;
      break;
   case GLUT_KEY_LEFT:
      Yrot -= step;
      break;
   case GLUT_KEY_RIGHT:
      Yrot += step;
      break;
   }
   glutPostRedisplay();
}


static void
Init(void)
{
   /* setup lighting, etc */
   glEnable(GL_DEPTH_TEST);
   glEnable(GL_LIGHTING);
   glEnable(GL_LIGHT0);
}


int
main(int argc, char *argv[])
{
   glutInit(&argc, argv);
   glutInitWindowSize(WinWidth, WinHeight);
   glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
   Win = glutCreateWindow(argv[0]);

   glh::glut_helpers_initialize();
   object.configure_buttons(1);

   object.dolly.dolly[2] = -15;
   glut_add_interactor(&cb);
   glut_add_interactor(&object);

   cb.keyboard_function = Key;
   cb.special_function = SpecialKey;
   cb.display_function = Draw;
   cb.keyboard_function = Key;
   cb.special_function = SpecialKey;
   cb.reshape_function = Reshape;


   if (Anim)
      glutIdleFunc(Idle);
   Init();

   glutMainLoop();
   return 0;
}

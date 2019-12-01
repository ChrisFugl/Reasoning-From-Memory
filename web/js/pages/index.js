import { Ask } from 'web/pages/Ask';
import { Main } from 'web/pages/main';
import { Tell } from 'web/pages/Tell';

const routes = [
  { component: Main, exact: true, path: '/' },
  { component: Ask, exact: false, path: '/ask' },
  { component: Tell, exact: false, path: '/tell' },
];

export { routes };

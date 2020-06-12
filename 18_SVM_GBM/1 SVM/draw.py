import numpy as np
import matplotlib.pyplot as plt




def draw(X, y, model = None, sv = True, l = True, n = 250, shape = (12, 8), size = 150, show = True, bk = True, lim = "auto"):
    fig, ax = plt.subplots(figsize = shape)
    ax.set_facecolor("#efefef")
    if lim == "auto":
        x_min, x_max = min(X[:, 0]) - 1, max(X[:, 0]) + 1
        y_min, y_max = min(X[:, 1]) - 1, max(X[:, 1]) + 1
    else:
        x_min = lim[0]
        x_max = lim[1]
        y_min = lim[2]
        y_max = lim[3]
    if model:
        xx = np.linspace(x_min, x_max, n)
        yy = np.linspace(y_min, y_max, n)
        XX, YY = np.meshgrid(xx, yy)
        #Z = model.decision_function(np.c_[XX.ravel(), YY.ravel()])
        if len(np.unique(y)) == 2:
            Z = model.decision_function(np.c_[XX.ravel(), YY.ravel()]).reshape(XX.shape)
        else:
            Z = model.predict(np.c_[XX.ravel(), YY.ravel()]).reshape(XX.shape)
        
        
        Z = Z.reshape(XX.shape)
        if bk:
            ax.pcolormesh(XX, YY, Z > 0, cmap="tab20c_r")
        ax.contour(XX, YY, Z, colors=["k"], linestyles=['-'], levels=[0], linewidths = [4])
        if l:
            ps = np.array([])
            for vector in model.support_vectors_:
                p = model.decision_function([vector])[0]
                ps = np.append(ps, p)
            if len(ps) > 0:
                p_neg = min(ps[ps<0])
                p_pos = max(ps[ps>0])
            ax.contour(XX, YY, Z, colors=["#333333"] * 2, linestyles=['--'] * 2, levels=[p_neg, p_pos], linewidths = [1] * 2)
        if sv:
            ax.scatter(model.support_vectors_[:, 0], model.support_vectors_[:, 1], s=size*2, facecolors='none', edgecolors='k')
    else:
        ax.axhline(0, color = "#eeeeee", zorder = -1)
        ax.axvline(0, color = "#eeeeee", zorder = -1)
    ax.scatter(X[:, 0], X[:, 1], c=y, cmap="bone", edgecolors='k', linewidth = 1, s = size, alpha = 1)
    ax.set_xlim(x_min, x_max)
    ax.set_ylim(y_min, y_max)
    if show:
        plt.show()
    else:
        return fig, ax




def draw3(X, y, model = None, sv = True, n = 250, shape = (12, 8), size = 150, show = True):
    fig, ax = plt.subplots(figsize = shape)
    x_min, x_max = min(X[:, 0]) - 1, max(X[:, 0]) + 1
    y_min, y_max = min(X[:, 1]) - 1, max(X[:, 1]) + 1
    if model:
        xx = np.linspace(x_min, x_max, n)
        yy = np.linspace(y_min, y_max, n)
        XX, YY = np.meshgrid(xx, yy)
        Z = model.predict(np.c_[XX.ravel(), YY.ravel()]).reshape(XX.shape)
        Z = Z.reshape(XX.shape)
        ax.pcolormesh(XX, YY, Z, cmap="Set3")
        ax.scatter(X[:, 0], X[:, 1], c=y, cmap="summer", edgecolors='k', linewidth = 1, s = size, alpha = 1)
        if sv:
            ax.scatter(model.support_vectors_[:, 0], model.support_vectors_[:, 1], s=size*2, facecolors='none', edgecolors='k')
    ax.set_xlim(x_min, x_max)
    ax.set_ylim(y_min, y_max)
    if show:
        plt.show()
    else:
        return fig, ax